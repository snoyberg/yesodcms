{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Feed
    ( getBlogFeedR
    , getContentFeedR
    , getContentFeedItemR
    , addFeedItem
    , addFeedItemText
    ) where

import Foundation
import Data.Time (getCurrentTime)
import Data.Text (Text)
import Yesod.Feed

getBlogFeedR :: Handler ()
getBlogFeedR = return ()

getContentFeedR :: Handler RepAtomRss
getContentFeedR = do
    now <- liftIO getCurrentTime

    entries <- runDB $ selectList [] [Desc FeedItemCreated, LimitTo 30] >>= (mapM $ \(nid, n) -> return FeedEntry
        { feedEntryLink = ContentFeedItemR nid
        , feedEntryUpdated = feedItemCreated n
        , feedEntryTitle = feedItemTitle n
        , feedEntryContent = feedItemContent n
        })

    newsFeed Feed
        { feedTitle = "Yesod Wiki" -- FIXME
        , feedLinkSelf = ContentFeedR
        , feedLinkHome = RootR
        , feedDescription = ""
        , feedLanguage = "en"
        , feedUpdated = now
        , feedEntries = entries
        }

getContentFeedItemR :: FeedItemId -> Handler ()
getContentFeedItemR fid = do
    f <- runDB $ get404 fid
    redirectText RedirectPermanent $ feedItemUrl f

addFeedItem :: Text -> CmsRoute -> [(Text, Text)] -> Html -> YesodDB Cms Cms ()
addFeedItem title route query content = do
    r <- lift getUrlRenderParams
    let url = r route query
    addFeedItemText title url content

addFeedItemText :: Text -> Text -> Html -> YesodDB Cms Cms ()
addFeedItemText title url content = do
    now <- liftIO getCurrentTime
    _ <- insert $ FeedItem now title url content
    return ()
