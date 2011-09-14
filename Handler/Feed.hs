{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Feed
    ( getBlogFeedR
    , getContentFeedR
    , getContentFeedItemR
    , addFeedItem
    , addFeedItemText
    , blogWidget
    ) where

import Foundation
import Data.Time (getCurrentTime)
import Data.Text (Text)
import Yesod.Feed
import FormatHandler
import FileStore
import qualified Data.Text as T

blogWidget :: Blog -> Widget
blogWidget b = do
    Cms { formatHandlers = fhs, fileStore = fs } <- lift getYesod
    let mfh = findHandler (snd $ T.breakOnEnd "." $ blogContents b) fhs
    muri <- liftIO $ fsGetFile fs $ blogContents b
    case (mfh, muri) of
        (Just fh, Just uri) -> fhFlatWidget fh (fsSM fs) uri
        _ -> [whamlet|<p>Format handler not found for #{blogContents b}|]

getBlogFeedR :: Handler RepAtomRss
getBlogFeedR = do
    now <- liftIO getCurrentTime
    r <- getUrlRenderParams

    blogs <- runDB $ selectList [] [Desc BlogPosted, LimitTo 3]
    entries <- mapM (\(_, b) -> do
        pc <- widgetToPageContent $ blogWidget b
        return FeedEntry
            { feedEntryLink = BlogPostR (blogYear b) (blogMonth b) (blogSlug b)
            , feedEntryUpdated = blogPosted b
            , feedEntryTitle = blogTitle b
            , feedEntryContent = pageBody pc r
            }) blogs

    newsFeed Feed
        { feedTitle = "Yesod Wiki" -- FIXME
        , feedLinkSelf = BlogFeedR
        , feedLinkHome = RootR
        , feedDescription = ""
        , feedLanguage = "en"
        , feedUpdated = now
        , feedEntries = entries
        }

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
