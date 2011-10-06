{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module FormatHandler.Html
    ( htmlFormatHandler
    , YesodAloha (..)
    , splitTitle
    , titleForm
    , alohaHtmlField
    ) where

import FormatHandler
import Settings (juliusFile)
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
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
import Text.Blaze (preEscapedText)
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Lazy as L
import Data.Enumerator (enumList)
import Text.HTML.TagSoup
import Control.Arrow ((***))
import Control.Applicative ((<$>), (<*>))
import Text.Blaze (Html)
import Network.URI.Enumerator

splitTitle :: T.Text -> (Maybe T.Text, T.Text)
splitTitle t =
    case T.stripPrefix "Title: " t of
        Just rest ->
            let (title, rest') = T.break (== '\n') rest
             in (Just title, T.drop 1 rest')
        Nothing -> (Nothing, t)

joinTitle :: (a -> T.Text) -> Maybe T.Text -> a -> T.Text
joinTitle unwrap Nothing t = unwrap t
joinTitle unwrap (Just a) t = T.concat ["Title: ", a, "\n", unwrap t]

titleForm :: RenderMessage master FormMessage
          => Field sub master a
          -> (T.Text -> a)
          -> (a -> T.Text)
          -> GWidget sub master ()
          -> Maybe T.Text
          -> Html
          -> Form sub master (FormResult T.Text, GWidget sub master ())
titleForm field wrap unwrap extraWidget mt =
    (fmap . fmap) (\(a, b) -> (a, b >> extraWidget))
      $ renderTable $ joinTitle unwrap
    <$> aopt textField "Title" (mtitle :: Maybe (Maybe T.Text))
    <*> areq field "Content" (fmap wrap content)
  where
    (mtitle, content) = maybe (Nothing, Nothing) ((Just *** Just) . splitTitle) mt

htmlFormatHandler :: (YesodAloha master, YesodJquery master) => FormatHandler master
htmlFormatHandler = FormatHandler
    { fhExts = Set.singleton "html"
    , fhName = "HTML"
    , fhForm = titleForm alohaHtmlField id id (return ())
    , fhWidget = widget
    , fhFilter = Just . enumList 8 . L.toChunks . TLE.encodeUtf8 . TL.fromStrict . sanitizeBalance . TL.toStrict . TLE.decodeUtf8With lenientDecode
    , fhRefersTo = const $ const $ return []
    , fhTitle = \sm uri -> fmap (fst . splitTitle) $ liftIO $ uriToText sm uri
    , fhFlatWidget = widget
    , fhToText = \sm uri -> fmap (Just . plain) $ liftIO $ uriToText sm uri
    , fhExtraParents = \_ _ -> return []
    }
  where
    widget sm uri = do
        (mtitle, t) <- fmap splitTitle $ liftIO $ uriToText sm uri
        let title = fromMaybe (snd $ T.breakOnEnd "/" $ fromMaybe (uriPath uri) $ T.stripSuffix "/index.html" $ uriPath uri) mtitle
        [whamlet|<h1>#{title}|]
        toWidget $ preEscapedText t
        let youtubes = mapMaybe (T.stripPrefix "http://www.youtube.com/watch?v=") $ hrefs t
        [whamlet|
$if not $ null youtubes
<div .videos>
    $forall y <- youtubes
        <div .video>
            <iframe width="560" height="315" src="http://www.youtube.com/embed/#{y}" frameborder="0" allowfullscreen>
|]
    plain = T.concat . mapMaybe plain' . parseTags
    plain' (TagText t) = Just t
    plain' _ = Nothing
    hrefs = mapMaybe hrefs' . parseTags
    hrefs' (TagOpen "a" as) = lookup "href" as
    hrefs' _ = Nothing

class YesodAloha a where
    urlAloha :: a -> [Either (Route a) T.Text]
    urlUpload :: a -> Route a

alohaHtmlField :: (YesodAloha master, YesodJquery master) => Field sub master T.Text
alohaHtmlField = Field
    { fieldParse = return . Right . fmap sanitizeBalance . listToMaybe
    , fieldView = \theId name val _isReq -> do
        y <- lift getYesod
        addScriptEither $ urlJqueryJs y
        mapM_ addScriptEither $ urlAloha y
        toWidget [shamlet|
<textarea ##{theId} name=#{name}>#{showVal val}
|]
        toWidget $(juliusFile "ckeditor")
        -- The next line is only for devices which do not support
        -- CKeditor
        toWidget [lucius|##{theId} { width: 500px; height: 300px; }|]
    }
  where
    showVal = either id id
