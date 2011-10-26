{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Search
    ( getSearchR
    , getSearchXmlpipeR
    , getLabels
    , DeviceGroup (..)
    , toDeviceGroups
    , liLabel
    ) where

import Foundation hiding (hamletFile)
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Search.Sphinx
import qualified Text.Search.Sphinx.Types as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import FileStore
import FormatHandler
import Data.Maybe (catMaybes)
import Control.Monad (forM, when)
import Database.Persist.Base
import qualified Text.Search.Sphinx.ExcerptConfiguration as E
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (ignore)
import Text.Blaze (preEscapedLazyText)
import Data.Enumerator (($$), run_, Enumerator, ($=), concatEnums, enumList, (=$), liftTrans)
import qualified Data.Enumerator.List as EL
import qualified Data.XML.Types as X
import Network.Wai (Response (ResponseEnumerator))
import Network.HTTP.Types (status200)
import Text.XML.Stream.Render (renderBuilder, def)
import Database.Persist.GenericSql (SqlPersist, runSqlPool)
import Handler.Profile (getLabels)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Hamlet (hamletFile)
import Handler.Cart (getCarts)
import Text.Blaze.Renderer.Text (renderHtml)
import qualified Data.Aeson as A
import Data.IORef (readIORef, atomicModifyIORef)
import Data.Time

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Convert a raw list of labels into a map grouped by the label group.
groupLabels :: [LabelInfo] -> GroupedLabels
groupLabels = Map.unionsWith Set.union . map (\li -> Map.singleton (liGroup li) $ Set.singleton li)

-- | Determine the label count for the set of MInfos.
getLabelCountI :: [MInfo]
               -> GroupedLabels  -- ^ already selected labels
               -> LabelInfo -- ^ what we're getting the count for
               -> Int
getLabelCountI minfos checked li =
    length $ filter (isIncluded checked') minfos
  where
    checked' = Map.insert (liGroup li) (Set.singleton li) checked

isIncluded :: GroupedLabels -> MInfo -> Bool
isIncluded groups mi =
    all go $ Map.toList groups
  where
    go (gid, lis) =
        case Map.lookup gid $ miLabels mi of
            Nothing -> False -- this is the bit of code that keeps going back and forth
            Just lis' -> not $ Set.null $ Set.intersection lis lis'

toLabelInfo :: LabelId -> YesodDB sub Cms (Maybe LabelInfo)
toLabelInfo lid = do
    ml <- get lid
    case ml of
        Nothing -> return Nothing
        Just l -> return $ Just $ LabelInfo (labelGroup l) lid (labelName l)

cachedQuery :: T.Text -> Handler (Either String ([MInfo], Bool))
cachedQuery query' = do
    $(logDebug) $ "cachedQuery: " `T.append` query'
    Cms { searchCache = isc } <- getYesod
    sc <- liftIO $ readIORef isc
    now <- liftIO getCurrentTime
    mres <-
        case Map.lookup query' sc of
            Nothing -> return Nothing
            Just (expire, mis, isVague) -> do
                if expire < now
                    then return Nothing
                    else return $ Just (mis, isVague)
    case mres of
        Just x -> do
            $(logDebug) $ "Using cached result, total results: " `T.append` T.pack (show $ length $ fst x)
            return $ Right x
        Nothing | T.null query' -> do
            $(logDebug) "Performing a null query, getting everything in database"
            misAll <- runDB getAllMInfos
            $(logDebug) $ T.pack $ "Got " ++ show (length misAll) ++ " results"
            let isVague = False
            liftIO $ atomicModifyIORef isc $ \m -> (Map.insert query' (cacheTime `addUTCTime` now, misAll, isVague) m, ())
            return $ Right (misAll, isVague)
        Nothing -> do
            res <- queryAll id 0
            case res of
                Right ms -> do
                    misAll <- runDB $ getMInfos ms query'
                    $(logDebug) $ "Caching new result, total results: " `T.append` T.pack (show $ length misAll)
                    let isVague = length misAll >= lim
                    liftIO $ atomicModifyIORef isc $ \m -> (Map.insert query' (cacheTime `addUTCTime` now, misAll, isVague) m, ())
                    return $ Right (misAll, isVague)
                Left s -> return $ Left s
  where
    cacheTime = 60 * 30 -- thirty minutes.. very arbitrary
    config = defaultConfig
        { port = 9312
        , mode = S.Any
        , limit = lim
        }
    lim = 1000
    queryAll front off = do
        res <- liftIO $ query config
            { offset = off
            } "yesodcms" $ T.unpack query'
        case res of
            S.Ok qr -> do
                let front' = front . (S.matches qr ++)
                -- FIXME
                return $ Right $ front' []
                {-
                if length (S.matches qr) == lim
                    then queryAll front' (off + lim)
                    else return $ Right $ front' []
                        -}
            x -> return $ Left $ show x

getSearchR :: Handler RepHtml
getSearchR = do
    $(logDebug) "Entering getSearchR"
    mquery <- runInputGet $ iopt textField "q"
    $(logDebug) "Finished querying Sphinx"
    gets <- fmap reqGetParams getRequest
    checkedLabels <- runDB $ fmap catMaybes $ mapM toLabelInfo $ mapMaybe (fromSinglePiece . snd) $ filter (\(x, _) -> x == "labels") gets
    let isChecked :: LabelId -> Bool
        isChecked = flip elem (map liLabel checkedLabels)
    let groupedCheckedLabels = groupLabels checkedLabels
    $(logDebug) "Finished grouping"
    let query' = fromMaybe "" mquery
    (resultsInner, misAll) <- do
        emisAll <- cachedQuery query'
        case emisAll of
            Left s -> return ([hamlet|<p>Error running search: #{s}|], [])
            Right (misAll, isVague) -> do
                let mis' = filter (isIncluded groupedCheckedLabels) misAll
                let mis = take 50 mis'
                let isClipped = length mis' > 50
                return (searchResultsInner mis isVague isClipped, misAll)
    let getLabelCount :: LabelInfo -> Widget
        getLabelCount label = do
            let len = getLabelCountI misAll groupedCheckedLabels label
            [whamlet|#{show len}|]
    isRaw <- runInputGet $ iopt boolField "raw"
    case isRaw of
        Just True -> do
            r <- getUrlRenderParams
            let str = TL.toStrict $ renderHtml $ resultsInner r
            labels <- runDB $ selectList [] []
            withCnt <- flip mapM labels $ \(lid, l) -> do
                let li = LabelInfo (labelGroup l) lid (labelName l)
                let i = getLabelCountI misAll groupedCheckedLabels li
                when (liName li `elem` ["AMS", "DeltaV", "Foundation Fieldbus", "HART"]) $ liftIO $ print (liName li, i)
                return (toSinglePiece lid, A.String $ T.pack $ show i)
            sendResponse $ RepJson $ toContent $ A.Object $ Map.fromList
                [ ("content", A.String str)
                , ("counts", A.Object $ Map.fromList withCnt)
                ]
        _ -> do
            labels <- runDB getLabels :: Handler [((GroupId, Group), [(LabelId, Label)])]
            labelInfos <- runDB $ selectList [] [] >>= mapM (\(lid, l) -> return $ LabelInfo (labelGroup l) lid (labelName l))
            let results = $(widgetFile "search-results")
            mcartTable <- do
                muid <- maybeAuthId
                case muid of
                    Nothing -> return Nothing
                    Just uid -> do
                        carts <- getCarts uid
                        let mmsg = Nothing :: Maybe String
                        return $ Just $ do
                            toWidget $(luciusFile "cart")
                            toWidget $(juliusFile "cart")
                            $(widgetFile "cart-table")
            defaultLayout $ do
                $(widgetFile "search-device-groups")
                $(widgetFile "search")
                $(widgetFile "comments")

getSearchXmlpipeR :: Handler RepXml
getSearchXmlpipeR = do
    Cms { formatHandlers = fhs, fileStore = fs, connPool = pool } <- getYesod
    let events = concatEnums
            [ enumList 8 startEvents
            , docEnum fhs fs
            , enumList 8 endEvents
            ]
    sendWaiResponse $ ResponseEnumerator $ \sriter -> do
        let iter = sriter status200 [("Content-Type", "text/xml")]
        flip runSqlPool pool $ run_ $ events $$ renderBuilder def =$ liftTrans iter
  where
    toName x = X.Name x (Just "http://sphinxsearch.com/") (Just "sphinx")
    docset = toName "docset"
    schema = toName "schema"
    field = toName "field"
    document = toName "document"
    content = "content" -- no prefix

    startEvents =
        [ X.EventBeginDocument
        , X.EventBeginElement docset []
        , X.EventBeginElement schema []
        , X.EventBeginElement field [("name", [X.ContentText "content"])]
        , X.EventEndElement field
        , X.EventEndElement schema
        ]

    endEvents =
        [ X.EventEndElement docset
        ]

    pairToEvents :: (FileNameId, T.Text) -> [X.Event]
    pairToEvents (fid, t) =
        [ X.EventBeginElement document [("id", [X.ContentText $ toSinglePiece fid])]
        , X.EventBeginElement content []
        , X.EventContent $ X.ContentText t
        , X.EventEndElement content
        , X.EventEndElement document
        ]

    docEnum :: [FormatHandler Cms] -> FileStore -> Enumerator X.Event (SqlPersist IO) a
    docEnum fhs fs = (selectEnum [] [] $= EL.concatMapM (helper fhs fs)) $= EL.concatMap pairToEvents

    helper :: [FormatHandler Cms] -> FileStore -> (FileNameId, FileName) -> SqlPersist IO [(FileNameId, T.Text)]
    helper fhs fs (fid, f) = do
        let t = T.drop 1 $ T.dropWhile (/= ':') $ fileNameUri f
        case findHandler (snd $ T.breakOnEnd "." t) fhs of
            Nothing -> return []
            Just fh -> do
                muri <- liftIO $ fsGetFile fs t
                case muri of
                    Nothing -> return []
                    Just uri -> do
                        mtext <- liftIO $ fhToText fh (fsSM fs) uri
                        case mtext of
                            Nothing -> return []
                            Just text -> do
                                title <- fileTitle' fs fhs t
                                update fid [FileNameTitle =. Just title, FileNameContent =. Just text]
                                return [(fid, T.concat [title, " ", text])]

getAllMInfos :: YesodDB sub Cms [MInfo]
getAllMInfos = do
    fs <- selectList [] []
    fmap catMaybes $ forM fs $ \(fid, f) -> do
        case f of
            FileName
                { fileNameTitle = Just title
                , fileNameUri = uri
                , fileNameContent = Just content'
                } -> do
                    let content = T.strip content'
                    labels <- selectList [FileLabelFile ==. fid] [] >>= mapM (toLabelInfo . fileLabelLabel . snd)
                    return $ Just MInfo
                        { miFile = T.drop 1 $ T.dropWhile (/= ':') uri
                        , miTitle = title
                        , miExcerpt = toHtml $
                            if T.length content > 100
                                then T.append (T.take 100 content) "..."
                                else content
                        , miLabels = groupLabels $ catMaybes labels
                        }
            _ -> return Nothing

getMInfos :: [S.Match] -> T.Text -> YesodDB sub Cms [MInfo]
getMInfos matches query' = fmap catMaybes $ forM matches $ \S.Match { S.documentId = did } -> do
    let fid = Key $ PersistInt64 did
    mf <- get fid
    case mf of
        Just FileName
            { fileNameTitle = Just title
            , fileNameContent = Just content
            , fileNameUri = uri
            } -> do
                let escape '<' = "&lt;"
                    escape '>' = "&gt;"
                    escape '&' = "&amp;"
                    escape c = T.singleton c
                rexcerpt <- liftIO $ buildExcerpts E.altConfig { E.port = 9312 } [T.unpack $ T.concatMap escape content] "yesodcms" $ T.unpack query'
                case rexcerpt of
                    S.Ok bss -> do
                        labels <- selectList [FileLabelFile ==. fid] [] >>= mapM (toLabelInfo . fileLabelLabel . snd)
                        return $ Just MInfo
                            { miFile = T.drop 1 $ T.dropWhile (/= ':') uri
                            , miTitle = title
                            , miExcerpt = preEscapedLazyText $ TL.concat $ map (decodeUtf8With ignore) bss
                            , miLabels = groupLabels $ catMaybes labels
                            }
                    _ -> return Nothing
        _ -> return Nothing

data DeviceGroup = DeviceGroup
    { dgName :: T.Text
    , dgLabels :: [(T.Text, LabelInfo)]
    }

toDeviceGroups :: [(LabelId, Label)] -> Maybe [DeviceGroup]
toDeviceGroups orig = do
    pairs <- mapM toPair orig
    let m = Map.unionsWith Map.union pairs
    return $ map (\(k, v) -> DeviceGroup k $ Map.toList v) $ Map.toList m
  where
    toPair (lid, l) = do
        let (x', y') = T.break (== '>') $ labelName l
        let x = T.strip x'
            y = T.strip $ T.drop 1 y'
        if T.null x || T.null y
            then Nothing
            else Just $ Map.singleton x $ Map.singleton y $ LabelInfo (labelGroup l) lid (labelName l)

searchResultsInner :: [MInfo] -> Bool -> Bool -> HtmlUrl CmsRoute
searchResultsInner mis isVague isClipped = $(hamletFile "hamlet/search-results-inner.hamlet")
