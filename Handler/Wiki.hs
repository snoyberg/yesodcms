{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Wiki
    ( getWikiR
    ) where

import Foundation
import qualified Data.Set as Set
import qualified Data.Text as T
import FormatHandler
import FileStore
import Data.Maybe (listToMaybe)
import Data.Enumerator (run_, ($$))
import Data.Enumerator.List (consume)
import qualified Data.ByteString.Lazy as L

getWikiR :: Texts -> Handler RepHtml
getWikiR pieces = do
    Cms { formatHandlers = fhs, fileStore = fs } <- getYesod
    file <- liftIO $ findFile fs fhs
    case file of
        Nothing -> defaultLayout $(widgetFile "create-wiki-page")
        Just (fh, ext, enum) -> do
            lbs <- liftIO $ fmap L.fromChunks $ run_ $ enum $$ consume
            let widget = fhWidget fh lbs
            defaultLayout $(widgetFile "show-wiki-page")
  where
    front ext = T.intercalate "/" $ pieces' ext
    pieces' ext = "wiki" : pieces ++ ["index." `T.append` ext]
    findFile _ [] = return Nothing
    findFile fs (fh:fhs) = do
        x <- findFile' fs fh (Set.toList $ fhExts fh)
        maybe (findFile fs fhs) (return . Just) x
    findFile' _ _ [] = return Nothing
    findFile' fs fh (e:es) = do
        x <- fsGetFile fs $ front e
        maybe (findFile' fs fh es) (\y -> return $ Just (fh, e, y)) x
