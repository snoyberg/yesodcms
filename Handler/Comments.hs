{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Comments
    ( getCommentsR
    , postCommentsR
    , getCommentCountR
    ) where

import Foundation
import Data.Aeson.Types (Value (Object, Number, String, Array))
import qualified Data.Map as Map
import Data.Vector (fromList)
import Text.Blaze.Renderer.Text (renderHtml)
import Data.Time
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import System.Locale
import Yesod.Goodies.Gravatar

getCommentCountR :: Handler RepJson
getCommentCountR = do
    element <- runInputGet $ ireq textField "element"
    x <- runDB $ count [CommentElement ==. element]
    jsonToRepJson $ Object $ Map.singleton "count" $ Number $ fromIntegral x

getCommentsR :: Handler RepJson
getCommentsR = do
    muid <- maybeAuthId
    element <- runInputGet $ ireq textField "element"
    comments <- runDB $ selectList [CommentElement ==. element] [Asc CommentTime] >>= (mapM $ \(_, c) -> do
        a <- get404 $ commentAuthor c
        return $ Object $ Map.fromList
            [ ("name", String $ userHandle a)
            , ("gravatar", String $ pack $ gravatarImg (userEmail a) defaultOptions
                { gSize = Just $ Size 40
                , gDefault = Just Identicon
                })
            , ("date", String $ prettyDateTime $ commentTime c)
            , ("content", String $ toStrict $ renderHtml $ toHtml $ commentContent c)
            ]
        )
    jsonToRepJson $ Object $ Map.fromList
        [ ("comments", Array $ fromList comments)
        , ("loggedin", jsonScalar $ maybe "false" (const "true") muid)
        ]

postCommentsR :: Handler ()
postCommentsR = do
    (uid, _u) <- requireAuth
    element <- runInputGet $ ireq textField "element"
    content <- runInputPost $ ireq textField "content"
    source <- runInputPost $ ireq textField "source"
    now <- liftIO getCurrentTime
    runDB $ do
        _ <- insert $ Comment element uid content now
        {- FIXME
        addNewsItem (T.concat
            [ userName u
            , " commented on "
            , topicTitle src
            ]) (TopicR topic) (Just hash) (toHtml content)
        -}
        return ()
    redirectText RedirectTemporary $ T.concat
        [ T.takeWhile (/= '#') source
        , "#"
        , element
        ]

prettyDateTime :: UTCTime -> Text
prettyDateTime = pack . formatTime defaultTimeLocale "%B %e, %y %l:%M %P"
