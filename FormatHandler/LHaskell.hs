{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module FormatHandler.LHaskell
    ( lhaskellFormatHandler
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
import FormatHandler.DITA (lhaskellToHTML)
import Text.Blaze (toHtml)

lhaskellFormatHandler :: FormatHandler master
lhaskellFormatHandler = FormatHandler
    { fhExts = Set.singleton "lhs"
    , fhName = "Literate Haskell"
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
        case lhaskellToHTML t of
            Nothing -> do
                id' <- lift newIdent
                toWidget [lucius|##{id'} { white-space: pre }|]
                toWidget [shamlet|<div ##{id'}>#{t}|]
            Just nodes -> toWidget $ mapM_ toHtml nodes
