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

textFormatHandler :: FormatHandler master
textFormatHandler = FormatHandler
    { fhExts = Set.singleton "txt"
    , fhName = "Plain text"
    , fhForm = (fmap . fmap) (\(a, b) -> (fmap unTextarea a, b >> toWidget css))
             . renderTable
             . areq textareaField "Content"
             . fmap Textarea
    , fhWidget = widget
    , fhFlatWidget = widget
    , fhFilter = Just . enumList 8 . L.toChunks
    , fhRefersTo = const $ const $ return []
    , fhTitle = \_ _ -> return Nothing
    }
  where
    css = [lucius|textarea { width: 500px; height: 400px } |]
    widget sm uri = do
        id' <- lift newIdent
        t <- liftIO $ uriToText sm uri
        toWidget [lucius|##{id'} { white-space: pre }|]
        toWidget [shamlet|<div ##{id'}>#{t}|]
