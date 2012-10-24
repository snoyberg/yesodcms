{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Comments
    ( getCommentsR
    , postCommentsR
    , getCommentCountR
    , prettyDateTime
    ) where

import Foundation
--import Data.Aeson.Types (Value (Object, Number, String, Array))
import Text.Blaze.Renderer.Text (renderHtml)
import Data.Time
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import System.Locale
import Network.Gravatar
import Handler.Feed (addFeedItemText)
import Data.Maybe (fromMaybe)

getCommentCountR :: Handler RepJson
getCommentCountR = do
    element <- runInputGet $ ireq textField "element"
    x <- runDB $ count [CommentElement ==. element]
    jsonToRepJson $ object ["count" .= x]

getCommentsR :: Handler RepJson
getCommentsR = do
    muid <- maybeAuthId
    element <- runInputGet $ ireq textField "element"
    comments <- runDB $ selectList [CommentElement ==. element] [Asc CommentTime] >>= (mapM $ \(Entity _ c) -> do
        a <- get404 $ commentAuthor c
        return $ object
            [ "name" .= toStrict (renderHtml $ userDisplayName a)
            , "gravatar" .= gravatarImg (userEmail a) defaultOptions
                { gSize = Just $ Size 40
                , gDefault = Just Identicon
                }
            , "date" .= prettyDateTime (commentTime c)
            , "content" .= toStrict (renderHtml $ toHtml $ commentContent c)
            ]
        )
    jsonToRepJson $ object
        [ "comments" .= array comments
        , "loggedin" .= maybe ("false" :: String) (const "true") muid
        ]

postCommentsR :: Handler ()
postCommentsR = do
    Entity uid u <- requireAuth
    element <- runInputGet $ ireq textField "element"
    content <- runInputPost $ ireq textField "content"
    source <- runInputPost $ ireq textField "source" -- FIXME add some sanity checking that this is the same site
    now <- liftIO getCurrentTime
    let dest = T.concat
            [ T.takeWhile (/= '#') source
            , "#"
            , element
            ]
    runDB $ do
        _ <- insert $ Comment element uid content now
        addFeedItemText (fromMaybe (userHandle u) (userName u) `T.append` " posted a comment") dest (toHtml content)
    redirect dest

prettyDateTime :: UTCTime -> Text
prettyDateTime = pack . formatTime defaultTimeLocale "%B %e, %Y %l:%M %P"
