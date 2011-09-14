{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module FormatHandler.Markdown
    ( markdownFormatHandler
    ) where

import FormatHandler
import Text.Lucius (lucius)
import Yesod.Form
import Yesod.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as L
import Data.Enumerator (enumList)
import Text.Pandoc (writeHtmlString, defaultWriterOptions, readMarkdown, defaultParserState)
import Text.Blaze (preEscapedString)
import qualified Data.Text as T

markdownFormatHandler :: FormatHandler master
markdownFormatHandler = FormatHandler
    { fhExts = Set.fromList ["md", "markdown"]
    , fhName = "Markdown"
    , fhForm = (fmap . fmap) (\(a, b) -> (fmap unTextarea a, b >> toWidget css))
             . renderTable
             . areq textareaField "Content"
             . fmap Textarea
    , fhWidget = widget
    , fhFlatWidget = widget
    , fhFilter = Just . enumList 8 . L.toChunks
    , fhRefersTo = const $ const $ return []
    , fhTitle = \_ _ -> return Nothing
    , fhToText = \sm uri -> fmap Just $ liftIO $ uriToText sm uri
    }
  where
    css = [lucius|textarea { width: 500px; height: 400px } |]
    widget sm uri = do
        t <- liftIO $ uriToText sm uri
        toWidget $ preEscapedString $ writeHtmlString defaultWriterOptions $ readMarkdown defaultParserState $ T.unpack $ T.filter (/= '\r') t
