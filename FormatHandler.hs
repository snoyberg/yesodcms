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
import Text.Blaze (Html, preEscapedText)
import Text.Lucius (lucius)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Text.Hamlet (shamlet)
import Data.Maybe (listToMaybe)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Julius (julius)
import Data.Enumerator (($$), run_)
import Data.Enumerator.List (consume)
import qualified Data.ByteString as S
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.URI.Enumerator
import Control.Monad (liftM)

data FormatHandler master = FormatHandler
    { fhExts :: Set.Set Ext
    , fhName :: T.Text
    , fhForm :: forall sub. RenderMessage master FormMessage => Maybe T.Text -> Html -> Form sub master (FormResult T.Text, GWidget sub master ())
    , fhWidget :: forall sub. SchemeMap IO -> URI -> GWidget sub master ()
    }

type Ext = T.Text

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

uriToText :: Monad m => SchemeMap m -> URI -> m T.Text
uriToText sm uri = liftM (decodeUtf8With lenientDecode . S.concat) $ run_ $ readURI sm uri $$ consume

htmlFormatHandler :: (YesodAloha master, YesodJquery master) => FormatHandler master
htmlFormatHandler = FormatHandler
    { fhExts = Set.singleton "html"
    , fhName = "HTML"
    , fhForm = renderTable . areq alohaHtmlField "Content"
    , fhWidget = \sm uri -> do
        t <- liftIO $ uriToText sm uri
        toWidget $ preEscapedText t
    }

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
        toWidget [lucius|##{theId}-container { width: 800px; height: 400px; }|]
    }
  where
    showVal = either id id

findHandler :: Ext -> [FormatHandler m] -> Maybe (FormatHandler m)
findHandler _ [] = Nothing
findHandler e (fh:fhs)
    | e `Set.member` fhExts fh = Just fh
    | otherwise = findHandler e fhs

ditaFormatHandler :: FormatHandler master
ditaFormatHandler = FormatHandler
    { fhExts = Set.fromList ["xml", "dita"]
    , fhName = "DITA Topic"
    , fhForm = (fmap . fmap) (\(a, b) -> (fmap unTextarea a, b >> toWidget css))
             . renderTable
             . areq textareaField "Content"
             . fmap Textarea
    , fhWidget = \_sm _uri -> do
        [whamlet|<h1>FIXME fhWidget of ditaFormatHandler|]
    }
  where
    css = [lucius|textarea { width: 500px; height: 400px } |]
