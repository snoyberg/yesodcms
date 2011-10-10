{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Search
    ( getSearchR
    , getSearchXmlpipeR
    , getLabels
    , DeviceGroup (..)
    , toDeviceGroups
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
import Control.Monad (forM)
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
import Settings.StaticFiles
import Text.Blaze.Renderer.Text (renderHtml)
import qualified Data.Aeson as A

data MInfo = MInfo
    { miFile :: T.Text
    , miTitle :: T.Text
    , miExcerpt :: TL.Text
    }

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Convert a raw list of labels into a map grouped by the label group.
groupLabels :: [LabelId] -> YesodDB Cms Cms (Map.Map GroupId (Set.Set LabelId))
groupLabels = fmap (Map.unionsWith Set.union) . mapM (\lid -> do
    ml <- get lid
    case ml of
        Nothing -> return Map.empty
        Just l -> return $ Map.singleton (labelGroup l) (Set.singleton lid))

-- | Ensure that the file in question exists and fulfills the filter.
verifyFile :: Map.Map GroupId (Set.Set LabelId) -> FileNameId -> YesodDB Cms Cms (Maybe FileName)
verifyFile groups fnid = do
    mfn <- get fnid
    case mfn of
        Nothing -> return Nothing
        Just fn -> go fn $ Map.toList groups
  where
    go fn [] = return $ Just fn
    go fn ((_, lids):gs) = do
        liftIO $ putStrLn ""
        liftIO $ print (fnid, lids)
        x <- selectList [FileLabelFile ==. fnid] []
        liftIO $ print x
        c <- count [FileLabelFile ==. fnid, FileLabelLabel <-. Set.toList lids]
        liftIO $ print c
        liftIO $ putStrLn ""
        if c == 0
            then return Nothing
            else go fn gs

getSearchR :: Handler RepHtml
getSearchR = do
    mquery <- runInputGet $ iopt textField "q"
    mres <- maybe (return Nothing) (fmap Just . liftIO . query config "yesodcms" . T.unpack) mquery
    gets <- fmap reqGetParams getRequest
    let checkedLabels = mapMaybe (fromSinglePiece . snd) $ filter (\(x, _) -> x == "labels") gets
    let isChecked = flip elem checkedLabels
    groupedCheckedLabels <- runDB $ groupLabels checkedLabels
    (matches, resultsInner) <-
            case mres of
                Nothing -> return ([], [hamlet||])
                Just (S.Ok qr) -> do
                    let ms = S.matches qr
                    mis <- getMInfos ms groupedCheckedLabels mquery
                    return (ms, $(hamletFile "hamlet/search-results-inner.hamlet"))
                Just x -> return ([], [hamlet|<p>Error running search: #{show x}|])
    let getLabelCountI :: LabelId -> Handler Int
        getLabelCountI label = do
            let cls =
                    if isChecked label
                        then checkedLabels -- FIXME remove all other labels that are in the same group
                        else label : checkedLabels
            gcls <- runDB $ groupLabels cls
            mis <- getMInfos matches gcls mquery
            return $ length mis
    let getLabelCount :: LabelId -> Widget
        getLabelCount label = do
            len <- lift $ getLabelCountI label
            [whamlet|#{show len}|]
    isRaw <- runInputGet $ iopt boolField "raw"
    case isRaw of
        Just True -> do
            r <- getUrlRenderParams
            let str = TL.toStrict $ renderHtml $ resultsInner r
            labels <- runDB $ selectList [] []
            withCnt <- flip mapM labels $ \(label, _) -> do
                i <- getLabelCountI label
                return (toSinglePiece label, A.String $ T.pack $ show i)
            sendResponse $ RepJson $ toContent $ A.Object $ Map.fromList
                [ ("content", A.String str)
                , ("counts", A.Object $ Map.fromList withCnt)
                ]
        _ -> do
            labels <- runDB getLabels
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
  where
    config = defaultConfig
        { port = 9312
        , mode = S.Any
        }

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

getMInfos :: [S.Match] -> Map.Map GroupId (Set.Set LabelId) -> Maybe T.Text -> Handler [MInfo]
getMInfos matches groupedCheckedLabels mquery = runDB $ fmap catMaybes $ forM matches $ \S.Match { S.documentId = did } -> do
    let fid = Key $ PersistInt64 did
    mf <- verifyFile groupedCheckedLabels fid
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
                rexcerpt <- liftIO $ buildExcerpts E.altConfig { E.port = 9312 } [T.unpack $ T.concatMap escape content] "yesodcms" $ T.unpack $ fromMaybe "" mquery
                case rexcerpt of
                    S.Ok bss ->
                        return $ Just MInfo
                            { miFile = T.drop 1 $ T.dropWhile (/= ':') uri
                            , miTitle = title
                            , miExcerpt = TL.concat $ map (decodeUtf8With ignore) bss
                            }
                    _ -> return Nothing
        _ -> return Nothing

data DeviceGroup = DeviceGroup
    { dgName :: T.Text
    , dgLabels :: [(T.Text, LabelId)]
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
            else Just $ Map.singleton x $ Map.singleton y lid
