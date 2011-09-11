{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module FormatHandler where

import qualified Data.Set as Set
import qualified Data.Text as T
import Yesod.Form
import Yesod.Form.Jquery
import Yesod.Core
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Text.Blaze (Html, preEscapedLazyText)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text.Encoding.Error (lenientDecode)
import Text.Lucius (lucius)
import Control.Monad.Trans.Class (lift)
import Text.Hamlet (shamlet)
import Data.Maybe (listToMaybe)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Julius (julius)
import Text.Blaze.Renderer.Text (renderHtml)

data FormatHandler master = FormatHandler
    { fhExts :: Set.Set Ext
    , fhName :: T.Text
    , fhForm :: forall sub. RenderMessage master FormMessage => Maybe L.ByteString -> Html -> Form sub master (FormResult L.ByteString, GWidget sub master ())
    , fhWidget :: forall sub. L.ByteString -> GWidget sub master ()
    }

type Ext = T.Text

textFormatHandler :: FormatHandler master
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

htmlFormatHandler :: (YesodAloha master, YesodJquery master) => FormatHandler master
htmlFormatHandler = FormatHandler
    { fhExts = Set.singleton "html"
    , fhName = "HTML"
    , fhForm = renderTable . areq alohaHtmlField "Content"
    , fhWidget = toWidget . preEscapedLazyText . TL.decodeUtf8With lenientDecode
    }

class YesodAloha a where
    urlAloha :: a -> Either (Route a) T.Text
    urlAlohaPlugins :: a -> [Either (Route a) T.Text]

alohaHtmlField :: (YesodAloha master, YesodJquery master) => Field sub master L.ByteString
alohaHtmlField = Field
    { fieldParse = return . Right . fmap (L.fromChunks . return . encodeUtf8 . sanitizeBalance) . listToMaybe
    , fieldView = \theId name val _isReq -> do
        y <- lift getYesod
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlAloha y
        mapM_ addScriptEither $ urlAlohaPlugins y
        toWidget [shamlet|
<div ##{theId}-container>
    <textarea ##{theId} name=#{name}>#{showVal val}
|]
        toWidget [julius|$(function(){$("##{theId}").aloha();})|]
        toWidget [lucius|##{theId}-container { width: 800px; height: 400px; }|]
    }
  where
    showVal = either id go
    go :: L.ByteString -> T.Text
    go = TL.toStrict . renderHtml . preEscapedLazyText . TL.decodeUtf8With lenientDecode

findHandler :: Ext -> [FormatHandler m] -> Maybe (FormatHandler m)
findHandler _ [] = Nothing
findHandler e (fh:fhs)
    | e `Set.member` fhExts fh = Just fh
    | otherwise = findHandler e fhs
