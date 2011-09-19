{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module FormatHandler.Text
    ( textFormatHandler
    ) where

import FormatHandler
import Text.Lucius (lucius)
import Yesod.Form
import Yesod.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import Text.Hamlet (shamlet)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as L
import Data.Enumerator (enumList)
import FormatHandler.Html (splitTitle, titleForm)

textFormatHandler :: FormatHandler master
textFormatHandler = FormatHandler
    { fhExts = Set.singleton "txt"
    , fhName = "Plain text"
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
        id' <- lift newIdent
        t <- fmap (snd . splitTitle) $ liftIO $ uriToText sm uri
        toWidget [lucius|##{id'} { white-space: pre }|]
        toWidget [shamlet|<div ##{id'}>#{t}|]
