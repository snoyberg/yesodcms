{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module FormatHandler.Html
    ( htmlFormatHandler
    , YesodAloha (..)
    ) where

import FormatHandler
import Text.Julius (julius)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Yesod.Core
import Yesod.Form
import Yesod.Form.Jquery
import Text.Lucius (lucius)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Text.Hamlet (shamlet)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Blaze (preEscapedText)
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Lazy as L
import Data.Enumerator (enumList)
import Text.HTML.TagSoup

htmlFormatHandler :: (YesodAloha master, YesodJquery master) => FormatHandler master
htmlFormatHandler = FormatHandler
    { fhExts = Set.singleton "html"
    , fhName = "HTML"
    , fhForm = renderTable . areq alohaHtmlField "Content"
    , fhWidget = widget
    , fhFilter = Just . enumList 8 . L.toChunks . TLE.encodeUtf8 . TL.fromStrict . sanitizeBalance . TL.toStrict . TLE.decodeUtf8With lenientDecode
    , fhRefersTo = const $ const $ return []
    , fhTitle = \_ _ -> return Nothing
    , fhFlatWidget = widget
    , fhToText = \sm uri -> fmap (Just . plain) $ liftIO $ uriToText sm uri
    , fhExtraParents = \_ _ -> return []
    }
  where
    widget sm uri = do
        t <- liftIO $ uriToText sm uri
        toWidget $ preEscapedText t
    plain = T.concat . mapMaybe plain' . parseTags
    plain' (TagText t) = Just t
    plain' _ = Nothing

class YesodAloha a where
    urlAloha :: a -> Either (Route a) T.Text
    urlAlohaPlugins :: a -> [Either (Route a) T.Text]

alohaHtmlField :: (YesodAloha master, YesodJquery master) => Field sub master T.Text
alohaHtmlField = Field
    { fieldParse = return . Right . fmap sanitizeBalance . listToMaybe
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
        toWidget [lucius|##{theId}-container { width: 800px; height: 400px; overflow: auto }|]
    }
  where
    showVal = either id id
