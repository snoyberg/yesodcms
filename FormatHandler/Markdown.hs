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
import FormatHandler.Html (splitTitle, titleForm)

markdownFormatHandler :: FormatHandler master
markdownFormatHandler = FormatHandler
    { fhExts = Set.fromList ["md", "markdown"]
    , fhName = "Markdown"
    , fhForm = titleForm textareaField Textarea unTextarea (toWidget css)
    , fhWidget = widget
    , fhFlatWidget = widget
    , fhFilter = Just . enumList 8 . L.toChunks
    , fhRefersTo = const $ const $ return []
    , fhTitle = \sm uri -> fmap (fst . splitTitle) $ liftIO $ uriToText sm uri
    , fhToText = \sm uri -> fmap Just $ liftIO $ uriToText sm uri
    , fhExtraParents = \_ _ -> return []
    }
  where
    css = [lucius|textarea { width: 500px; height: 400px } |]
    widget sm uri = do
        t <- fmap (snd . splitTitle) $ liftIO $ uriToText sm uri
        toWidget $ preEscapedString $ writeHtmlString defaultWriterOptions $ readMarkdown defaultParserState $ T.unpack $ T.filter (/= '\r') t
