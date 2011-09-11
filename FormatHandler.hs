{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module FormatHandler where

import qualified Data.Set as Set
import qualified Data.Text as T
import Yesod.Form
import Yesod.Handler (newIdent)
import Yesod.Widget (GWidget, toWidget)
import Yesod.Message (RenderMessage)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Text.Blaze (Html)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text.Encoding.Error (lenientDecode)
import Text.Lucius (lucius)
import Control.Monad.Trans.Class (lift)
import Text.Hamlet (shamlet)

data FormatHandler = FormatHandler
    { fhExts :: Set.Set Ext
    , fhName :: T.Text
    , fhForm :: forall sub master. RenderMessage master FormMessage => Maybe L.ByteString -> Html -> Form sub master (FormResult L.ByteString, GWidget sub master ())
    , fhWidget :: forall sub master. L.ByteString -> GWidget sub master ()
    }

type Ext = T.Text

textFormatHandler :: FormatHandler
textFormatHandler = FormatHandler
    { fhExts = Set.singleton "txt"
    , fhName = "Plain text"
    , fhForm = (fmap . fmap) (\(a, b) -> (fmap (L.fromChunks . return . encodeUtf8 . unTextarea) a, b >> toWidget css))
             . renderTable
             . areq textareaField "Content"
             . fmap (Textarea . decodeUtf8With lenientDecode . S.concat . L.toChunks)
    , fhWidget = \lbs -> do
        id' <- lift newIdent
        toWidget [lucius|##{id'} { white-space: pre }|]
        toWidget [shamlet|<div ##{id'}>#{TL.decodeUtf8With lenientDecode lbs}|]
    }
  where
    css = [lucius|textarea { width: 500px; height: 400px } |]

findHandler :: Ext -> [FormatHandler] -> Maybe FormatHandler
findHandler _ [] = Nothing
findHandler e (fh:fhs)
    | e `Set.member` fhExts fh = Just fh
    | otherwise = findHandler e fhs
