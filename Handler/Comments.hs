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
import Handler.Feed (addFeedItemText)
import Data.Maybe (fromMaybe)

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
            [ ("name", String $ toStrict $ renderHtml $ userDisplayName a)
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
    (uid, u) <- requireAuth
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
    redirectText RedirectTemporary dest

prettyDateTime :: UTCTime -> Text
prettyDateTime = pack . formatTime defaultTimeLocale "%B %e, %y %l:%M %P"
