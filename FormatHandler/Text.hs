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

textFormatHandler :: FormatHandler master
textFormatHandler = FormatHandler
    { fhExts = Set.singleton "txt"
    , fhName = "Plain text"
    , fhForm = (fmap . fmap) (\(a, b) -> (fmap unTextarea a, b >> toWidget css))
             . renderTable
             . areq textareaField "Content"
             . fmap Textarea
    , fhWidget = \sm uri -> do
        id' <- lift newIdent
        t <- liftIO $ uriToText sm uri
        toWidget [lucius|##{id'} { white-space: pre }|]
        toWidget [shamlet|<div ##{id'}>#{t}|]
    }
  where
    css = [lucius|textarea { width: 500px; height: 400px } |]
