module Handler.Page
    ( getPageR
    , getPageR'
    ) where

import Import
import qualified Data.Text as T
import Handler.Wiki (findFile)
import FileStore
import FormatHandler
import Control.Monad (unless)
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)

getPageR :: T.Text -> [T.Text] -> Handler RepHtml
getPageR x xs = getPageR' $ x:xs

getPageR' :: [T.Text] -> Handler RepHtml
getPageR' ts = do
    mu <- maybeAuth
    let canWrite = fmap (userAdmin . entityVal) mu == Just True
    Cms { formatHandlers = fhs, fileStore = fs } <- getYesod
    mfile <- liftIO $ findFile "page" ts fs fhs
    case mfile of
        Just (fh, ext, enum) -> do
            mtitle <- liftIO $ fhTitle fh (fsSM fs) enum
            let widget = fhWidget fh (fsSM fs) enum
            defaultLayout $ do
                maybe (return ()) (setTitle . toHtml) mtitle
                $(widgetFile "show-page")
        Nothing -> do
            unless canWrite notFound
            defaultLayout $(widgetFile "create-wiki-page")
  where
    pieces' ext = "page" : ts ++ ["index." `T.append` ext]
