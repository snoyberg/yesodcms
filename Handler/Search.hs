{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Search
    ( getSearchR
    , getLabels
    , DeviceGroup (..)
    , toDeviceGroups
    , liLabel
    ) where

import Foundation hiding (hamletFile)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Maybe (catMaybes)
import Control.Monad (forM, when)
import Database.Persist.Base
import Database.Persist.GenericSql.Raw (withStmt)
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (ignore)
import Text.Blaze (preEscapedText)
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
            res <- queryAll id (0 :: Int)
            case res of
                Right misAll -> do
                    --misAll <- runDB $ getMInfos ms query'
                    $(logDebug) $ "Caching new result, total results: " `T.append` T.pack (show $ length misAll)
                    let isVague = False -- length misAll >= lim
                    liftIO $ atomicModifyIORef isc $ \m -> (Map.insert query' (cacheTime `addUTCTime` now, misAll, isVague) m, ())
                    return $ Right (misAll, isVague)
                Left s -> return $ Left s
  where
    cacheTime = 60 * 30 -- thirty minutes.. very arbitrary
    queryAll _ _ = do
        let queryE = T.replace "'" "''" query'
        let sql = T.concat
                [ "SELECT id, uri, title, ts_headline('english', content, plainto_tsquery('"
                , queryE
                , "')) FROM \"FileName\" WHERE to_tsvector(content) @@ plainto_tsquery('"
                , queryE
                , "') ORDER BY ts_rank(to_tsvector(content), plainto_tsquery('"
                , queryE
                , "'))"
                ]
        let loop front x = do
                y <- x
                case y of
                    Nothing -> return $ Right $ front []
                    Just [PersistInt64 fid', PersistByteString url', PersistByteString title', PersistByteString excerpt'] -> do
                        let url = T.drop 1 $ T.dropWhile (/= ':') $ TE.decodeUtf8With ignore url'
                        let title = TE.decodeUtf8With ignore title'
                        let excerpt = preEscapedText $ TE.decodeUtf8With ignore excerpt'
                        let fid = Key $ PersistInt64 fid'
                        labels <- selectList [FileLabelFile ==. fid] [] >>= mapM (toLabelInfo . fileLabelLabel . snd)
                        let mi = MInfo
                                { miFile = url
                                , miTitle = title
                                , miExcerpt = excerpt
                                , miLabels = groupLabels $ catMaybes labels
                                }
                        loop (front . (mi:)) x
                    Just _ -> loop front x
        runDB $ withStmt sql [] $ loop id

getSearchR :: Handler RepHtml
getSearchR = do
    mquery <- runInputGet $ iopt textField "q"
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
