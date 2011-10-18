{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root
    ( getRootR
    , getAddArticleR
    , postAddArticleR
    , getAddVideoR
    , postAddVideoR
    , getArticlesR
    , getHowToArticlesR
    , getVideosR
    ) where

import Foundation
import Handler.Search (getLabels, DeviceGroup (..), toDeviceGroups, liLabel)
import Yesod.Auth (apLogin)
import Data.Text (Text)
import FormatHandler.Html (alohaHtmlField)
import Control.Applicative
import qualified Data.Text as T
import FileStore
import GoogleEmail
import Data.Enumerator (enumList)
import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import Data.Char (toLower)
import System.Random.Mersenne
import Data.Word (Word)
import Data.Time
import Data.Maybe (fromMaybe)
import Handler.Comments (prettyDateTime)
import Handler.Cart (getCartWidget)
import FormatHandler.Video
import qualified Data.Text.Encoding as TE

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

data ArticleInfo = ArticleInfo
    { aiTitle :: Text
    , aiLink :: CmsRoute
    , aiUser :: Text
    , aiLabels :: Text
    , aiTimestamp :: Text
    , aiLids :: [LabelId]
    }

getArticleInfo :: Article -> YesodDB Cms Cms ArticleInfo
getArticleInfo a = do
    cr <- lift getCurrentRoute
    let isHome = cr == Just RootR
    u <- get404 $ articleUser a
    lids <- fmap (map $ fileLabelLabel . snd) $ selectList [FileLabelFile ==. articleFile a] []
    labels <- mapM get404 lids
    return ArticleInfo
        { aiTitle = articleTitle a
        , aiLink = WikiR [articleName a]
        , aiUser = fromMaybe (userHandle u) (userName u)
        , aiLabels = T.intercalate "; " $ (if isHome then ellipsis 4 else id) $ map labelName labels
        , aiTimestamp = prettyDateTime $ articleAdded a
        , aiLids = lids
        }

ellipsis :: Int -> [Text] -> [Text]
ellipsis _ [] = []
ellipsis 0 _ = ["..."]
ellipsis i (x:xs) = x : ellipsis (i - 1) xs

addArticleForm :: Html -> Form Cms Cms (FormResult (Text, Text), Widget)
addArticleForm html = do
    (title, tview) <- mreq textField "Title" Nothing
    (content, cview) <- mreq alohaHtmlField "Content"
            { fsId = Just "aloha"
            } Nothing
    let widget = [whamlet|
<tr>
    <td colspan=2>
        Title: #
        ^{fvInput tview}
        \#{html}
<tr>
    <td colspan=2>
        ^{fvInput cview}
|]
    return ((,) <$> title <*> content, widget)

getRootR :: Handler RepHtml
getRootR = do
    muid <- maybeAuthId
    let mcart = fmap (getCartWidget False) muid
    labels <- runDB getLabels
    ((_, addArticleWidget), _) <- runFormPost addArticleForm
    ((_, addVideoWidget), _) <- runFormPost $ videoForm Nothing
    let articleLink a = WikiR [articleName a]
    articles <- runDB $ selectList [] [Desc ArticleAdded, LimitTo 5] >>= mapM (getArticleInfo . snd)
    defaultLayout $ do
        $(widgetFile "search-device-groups")
        $(widgetFile "root")

getAddArticleR :: Handler RepHtml
getAddArticleR = do
    uid <- requireAuthId
    ((res, widget), _) <- runFormPost addArticleForm
    case res of
        FormSuccess (title, content) -> do
            num <- liftIO randomIO
            let name = T.pack (show $ (num :: Word) `mod` 10000) `T.append` "-" `T.append` toSlug title
            let path = T.concat ["wiki/", name, "/index.html"]
            fs <- fmap fileStore getYesod
            liftIO $ fsPutFile fs path $ enumList 8 $ L.toChunks $ encodeUtf8 $ TL.fromChunks
                [ "Title: "
                , T.takeWhile (/= '\n') title
                , "\n"
                , content
                ]
            fid <- runDB $ getFileNameId path
            now <- liftIO getCurrentTime
            runDB $ insert (Article title name now uid fid) >> addLabel fid "How to Article"
            setMessage "Your article has been added. You can now set labels on the article."
            r <- getUrlRenderParams
            redirectText RedirectTemporary $ r (EditPageR ["wiki", name, "index.html"]) [("labels", "yes")]
        _ -> defaultLayout $(widgetFile "add-article")

postAddArticleR :: Handler RepHtml
postAddArticleR = getAddArticleR

toSlug :: Text -> Text
toSlug =
    T.concatMap go
  where
    go c
        | 'A' <= c && c <= 'Z' = T.singleton $ toLower c
        | 'a' <= c && c <= 'z' = T.singleton c
        | '0' <= c && c <= '9' = T.singleton c
    go '-' = "-"
    go ' ' = "-"
    go '_' = "-"
    go _ = ""

getAddVideoR :: Handler RepHtml
getAddVideoR = do
    uid <- requireAuthId
    ((res, widget), _) <- runFormPost $ videoForm Nothing
    case res of
        FormSuccess text -> do
            num <- liftIO randomIO
            Just (Video title _ _) <- return $ parseVideo text
            let name = T.pack (show $ (num :: Word) `mod` 10000) `T.append` "-" `T.append` toSlug title
            let path = T.concat ["wiki/", name, "/index.video"]
            fs <- fmap fileStore getYesod
            liftIO $ fsPutFile fs path $ enumList 1 [TE.encodeUtf8 text]
            fid <- runDB $ getFileNameId path
            now <- liftIO getCurrentTime
            runDB $ insert (Article title name now uid fid) >> addLabel fid "Video"
            setMessage "Your video has been added. You can now set labels on the video."
            r <- getUrlRenderParams
            redirectText RedirectTemporary $ r (EditPageR ["wiki", name, "index.video"]) [("labels", "yes")]
        _ -> defaultLayout $(widgetFile "add-video")

postAddVideoR :: Handler RepHtml
postAddVideoR = getAddVideoR

getArticlesR :: Handler RepHtml
getArticlesR = do
    articles <- runDB $ selectList [] [Desc ArticleAdded, LimitTo 30] >>= mapM (getArticleInfo . snd)
    defaultLayout $ do
        toWidget $(luciusFile "root")
        $(widgetFile "articles")

getHowToArticlesR :: Handler RepHtml
getHowToArticlesR = getFiltered "How to Article"

getVideosR :: Handler RepHtml
getVideosR = getFiltered "Video"

getFiltered :: T.Text -> Handler RepHtml
getFiltered t = do
    articles <- runDB $ do
        ml <- selectFirst [LabelName ==. t] []
        lid <- fmap fst $ maybe (lift notFound) return ml
        ais <- selectList [] [Desc ArticleAdded, LimitTo 50] >>= mapM (getArticleInfo . snd)
        return $ filter (\ai -> lid `elem` aiLids ai) ais
    defaultLayout $ do
        toWidget $(luciusFile "root")
        $(widgetFile "articles")
