module Handler.Comments
    ( getCommentsR
    , postCommentsR
    , getCommentCountR
    , prettyDateTime
    ) where

import Import
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Time (UTCTime, getCurrentTime, formatTime)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import System.Locale (defaultTimeLocale)
import Network.Gravatar
import Handler.Feed (addFeedItemText)
import Data.Maybe (fromMaybe)
import Data.Default (Default (..))

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
            , "gravatar" .= gravatar opts (userEmail a)
            , "date" .= prettyDateTime (commentTime c)
            , "content" .= toStrict (renderHtml $ toHtml $ commentContent c)
            ]
        )
    jsonToRepJson $ object
        [ "comments" .= array comments
        , "loggedin" .= maybe ("false" :: String) (const "true") muid
        ]
    where
       opts = def
           { gSize = Just $ Size 40
           , gDefault = Just Identicon
           }

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
prettyDateTime = T.pack . formatTime defaultTimeLocale "%B %e, %Y %l:%M %P"
