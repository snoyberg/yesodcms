module FormatHandler.Html
    ( htmlFormatHandler
    , YesodAloha (..)
    , splitTitle
    , titleForm
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
import Control.Monad.IO.Class (liftIO)
import Text.Hamlet (shamlet)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Blaze.Html (preEscapedToHtml)
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Lazy as L
import Data.Conduit.List (sourceList)
import Text.HTML.TagSoup
import Control.Arrow ((***))
import Control.Applicative ((<$>), (<*>))
import Text.Blaze.Html (Html)
import Prelude

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
          -> MForm sub master (FormResult T.Text, GWidget sub master ())
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
    , fhFilter = Just . sourceList . L.toChunks . TLE.encodeUtf8 . TL.fromStrict . sanitizeBalance . TL.toStrict . TLE.decodeUtf8With lenientDecode
    , fhRefersTo = const $ const $ return []
    , fhTitle = \sm uri -> fmap (fst . splitTitle) $ liftIO $ uriToText sm uri
    , fhFlatWidget = widget
    , fhToText = \sm uri -> fmap (Just . plain) $ liftIO $ uriToText sm uri
    , fhExtraParents = \_ _ -> return []
    }
  where
    widget sm uri = do
        t <- fmap (snd . splitTitle) $ liftIO $ uriToText sm uri
        toWidget $ preEscapedToHtml t
    plain = T.concat . mapMaybe plain' . parseTags
    plain' (TagText t) = Just t
    plain' _ = Nothing

class YesodAloha a where
    urlAloha :: a -> Either (Route a) T.Text
    urlAlohaPlugins :: a -> [Either (Route a) T.Text]

alohaHtmlField :: (YesodAloha master, YesodJquery master) => Field sub master T.Text
alohaHtmlField = Field
    { fieldParse = return . Right . fmap sanitizeBalance . listToMaybe
    , fieldView = \theId name _classes val _isReq -> do
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
