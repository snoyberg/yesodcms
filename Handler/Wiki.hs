{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Wiki
    ( getWikiR
    , findFile
    ) where

import Foundation
import qualified Data.Set as Set
import qualified Data.Text as T
import FormatHandler
import FileStore
import Data.Maybe (listToMaybe)
import Data.Enumerator (Enumerator)
import Data.ByteString (ByteString)

getWikiR :: Texts -> Handler RepHtml
getWikiR pieces = do
    Cms { formatHandlers = fhs, fileStore = fs } <- getYesod
    file <- liftIO $ findFile "wiki" pieces fs fhs
    case file of
        Nothing -> defaultLayout $(widgetFile "create-wiki-page")
        Just (fh, ext, enum) -> do
            let widget = fhWidgetEnum enum fh
            defaultLayout $(widgetFile "show-wiki-page")
  where
    front ext = T.intercalate "/" $ pieces' ext
    pieces' ext = "wiki" : pieces ++ ["index." `T.append` ext]

findFile :: T.Text -> [T.Text] -> FileStore -> [FormatHandler m] -> IO (Maybe (FormatHandler m, T.Text, Enumerator ByteString IO a))
findFile _ _ _ [] = return Nothing
findFile first pieces fs (fh:fhs) = do
    x <- findFile' (Set.toList $ fhExts fh)
    maybe (findFile first pieces fs fhs) (return . Just) x
  where
    front ext = T.intercalate "/" $ pieces' ext
    pieces' ext = first : pieces ++ ["index." `T.append` ext]
    findFile' [] = return Nothing
    findFile' (e:es) = do
        x <- fsGetFile fs $ front e
        maybe (findFile' es) (\y -> return $ Just (fh, e, y)) x
