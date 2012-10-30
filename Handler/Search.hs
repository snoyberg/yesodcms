module Handler.Search
    ( getSearchR
    ) where

import Import
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Search.Sphinx
import qualified Text.Search.Sphinx.Types as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Maybe (catMaybes)
import Control.Monad (forM)
import Database.Persist.Store (PersistValue(PersistInt64))
import qualified Text.Search.Sphinx.ExcerptConfiguration as E
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (ignore)
import Handler.Profile (getLabels)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Blaze.Html (preEscapedToHtml)

data MInfo = MInfo
    { miFile :: T.Text
    , miTitle :: T.Text
    , miExcerpt :: TL.Text
    }

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
    let checkedLabels = mapMaybe (fromPathPiece . snd) $ filter (\(x, _) -> x == "labels") gets
    let isChecked = flip elem checkedLabels
    groupedCheckedLabels <- runDB $ groupLabels checkedLabels
    labels <- runDB getLabels
    let results =
            case mres of
                Nothing -> return ()
                Just (S.Ok qr) -> do
                    mis <- lift $ runDB $ fmap catMaybes $ forM (S.matches qr) $ \S.Match { S.documentId = did } -> do
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
                    $(widgetFile "search-results")
                Just x -> [whamlet|<p>Error running search: #{show x}|]
    defaultLayout $(widgetFile "search")
  where
    config = defaultConfig
        { port = 9312
        , mode = S.Any
        }
