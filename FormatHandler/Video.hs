{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module FormatHandler.Video
    ( videoFormatHandler
    , videoForm
    , Video (..)
    , parseVideo
    ) where

import FormatHandler
import Yesod.Form
import Yesod.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import Text.Hamlet (shamlet)
import qualified Data.ByteString.Lazy as L
import Data.Enumerator (enumList)
import qualified Data.Text as T
import Network.URI.Enumerator
import Text.Blaze (Html)
import Control.Applicative

data Video = Video
    { videoTitle :: T.Text
    , videoId :: T.Text
    , videoDesc :: Textarea
    }

parseVideo :: T.Text -> Maybe Video
parseVideo t =
    case T.lines t of
        (a:b:rest) -> Just $ Video a b $ Textarea $ T.unlines rest
        _ -> Nothing

renderVideo :: Video -> T.Text
renderVideo (Video a b (Textarea c)) = T.concat [a, "\n", b, "\n", c]

videoFormatHandler :: RenderMessage master FormMessage => FormatHandler master
videoFormatHandler = FormatHandler
    { fhExts = Set.singleton "video"
    , fhName = "Video"
    , fhForm = videoForm
    , fhWidget = widget
    , fhFilter = Just . enumList 8 . L.toChunks
    , fhRefersTo = const $ const $ return []
    , fhTitle = \sm uri -> fmap (fmap videoTitle . parseVideo) $ liftIO $ uriToText sm uri
    , fhFlatWidget = widget
    , fhToText = \sm uri -> fmap Just $ liftIO $ uriToText sm uri
    , fhExtraParents = \_ _ -> return []
    }

videoForm :: RenderMessage master FormMessage => Maybe T.Text -> Html -> Form sub master (FormResult T.Text, GWidget sub master ())
videoForm minput = renderTable $ (\x y z -> renderVideo $ Video x y z)
    <$> areq textField "Title" (videoTitle <$> mvideo)
    <*> areq (check toId textField) "ID"
            { fsTooltip = Just "Copy the Youtube link"
            } (videoId <$> mvideo)
    <*> areq textareaField "Description" (videoDesc <$> mvideo)
  where
    mvideo = minput >>= parseVideo
    prefixes =
        [ "http://www.youtube.com/watch?v="
        , "https://www.youtube.com/watch?v="
        ]
    toId t =
        go prefixes
      where
        go [] = Right t :: Either T.Text T.Text
        go (p:ps) =
            case T.stripPrefix p t of
                Just x -> Right $ T.takeWhile (\c -> not $ c `elem` "&#") x
                Nothing -> go ps

widget :: SchemeMap IO -> URI -> GWidget sub master ()
widget sm uri = do
    mvideo <- fmap parseVideo $ liftIO $ uriToText sm uri
    case mvideo of
        Nothing -> toWidget [shamlet|<p>Error reading video file|]
        Just (Video title vid desc) -> toWidget [shamlet|
<h1>#{title}
<p style=white-space:pre-wrap>#{desc}
<iframe width="560" height="315" src="http://www.youtube.com/embed/#{vid}" frameborder="0" allowfullscreen>
|]
