{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Cart
    ( getCartR
    , getCartWidget
    , postAddCartR
    , postUpCartR
    , postDownCartR
    , postDeleteCartR
    , getCartPrintR
    , getCartPdfR
    , getCartEpubR
    ) where

import Foundation
import qualified Data.Text as T
import Handler.EditPage (getFileNameId)
import Control.Monad (unless)
import Data.Monoid (mconcat)
import FileStore
import FormatHandler
import Settings.StaticFiles
import System.Random.Mersenne
import System.Directory (createDirectoryIfMissing)
import Data.Word (Word)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy as L
import System.Cmd (rawSystem)
import Epub (epub)

getCartWidget :: Bool -> UserId -> Widget
getCartWidget title uid = do
    Cms { formatHandlers = fhs, fileStore = fs } <- lift getYesod
    carts <- lift $ runDB $ selectList [CartUser ==. uid] [Asc CartPriority] >>= mapM (\(cid, c) -> do
        file <- get404 $ cartFile c
        let t = T.drop 3 $ fileNameUri file
        title' <-
            case fileNameTitle file of
                Just t' -> return t'
                Nothing -> fileTitle' fs fhs t
        return (cid, (RedirectorR t, title'))
        )
    $(widgetFile "cart")

getCartR :: Handler RepHtml
getCartR = requireAuthId >>= defaultLayout . getCartWidget True

postAddCartR :: T.Text -> Handler ()
postAddCartR t = do
    uid <- requireAuthId
    msg <- runDB $ do
        fid <- getFileNameId t
        x <- insertBy $ Cart uid fid 100000
        case x of
            Left{} -> return "That topic is already in your documents"
            Right{} -> do
                fixCarts uid
                return "MyDocs updated!"
    setMessage msg
    redirect RedirectTemporary CartR

fixCarts :: UserId -> YesodDB Cms Cms ()
fixCarts uid = do
    keys <- fmap (map fst) $ selectList [CartUser ==. uid] [Asc CartPriority]
    mapM_ (\(key, priority) -> update key [CartPriority =. priority]) $ zip keys [10, 20..]

postUpCartR, postDownCartR, postDeleteCartR :: CartId -> Handler ()
postUpCartR cid = do
    uid <- requireOwner cid
    runDB $ update cid [CartPriority -=. 15] >> fixCarts uid
    setMessage "Document moved up"
    redirect RedirectTemporary CartR
postDownCartR cid = do
    uid <- requireOwner cid
    runDB $ update cid [CartPriority +=. 15] >> fixCarts uid
    setMessage "Document moved down"
    redirect RedirectTemporary CartR
postDeleteCartR cid = do
    uid <- requireOwner cid
    runDB $ delete cid >> fixCarts uid
    setMessage "Document deleted"
    redirect RedirectTemporary CartR

requireOwner :: CartId -> Handler UserId
requireOwner cid = do
    uid <- requireAuthId
    c <- runDB $ get404 cid
    unless (uid == cartUser c) $ permissionDenied "That's not your document"
    return uid

getCartHtml :: Handler Html
getCartHtml = do
    uid <- requireAuthId
    Cms { formatHandlers = fhs, fileStore = fs } <- getYesod
    widgets <- runDB $ selectList [CartUser ==. uid] [Asc CartPriority] >>= mapM (\(_, c) -> do
        file <- get404 $ cartFile c
        let t = T.drop 3 $ fileNameUri file
        title <-
            case fileNameTitle file of
                Just t' -> return t'
                Nothing -> fileTitle' fs fhs t

        muri <- liftIO $ fsGetFile fs t
        let ext = snd $ T.breakOnEnd "." t
        let mfh = findHandler ext fhs
        let widget =
                case (muri, mfh) of
                    (Just uri, Just fh) -> fhFlatWidget fh (fsSM fs) uri
                    _ -> [whamlet|<p>Error producing output|]

        return [whamlet|
<h1>#{title}
^{widget}
|]
        )
    pc <- widgetToPageContent $ mconcat widgets
    r <- getUrlRenderParams
    return $ [hamlet|
!!!
<html>
    <head>
        <title>MyDocs
    <body>
        ^{pageBody pc}
|] r

getCartPrintR :: Handler RepHtml
getCartPrintR = fmap (RepHtml . toContent) getCartHtml

getCartPdfR :: Handler ()
getCartPdfR = do
    html <- getCartHtml
    liftIO $ createDirectoryIfMissing True "tmp"
    num <- liftIO randomIO
    let name = show $ (num :: Word) `mod` 1000000
    let htmlFile = concat ["tmp/", name, ".html"]
        pdfFile = concat ["tmp/", name, ".pdf"]
    liftIO $ L.writeFile htmlFile $ renderHtml html
    _ <- liftIO $ rawSystem "wkhtmltopdf" [htmlFile, pdfFile]
    setHeader "Content-disposition" "attachment; filename=MyDocs.pdf"
    sendFile "application/pdf" pdfFile

getCartEpubR :: Handler (ContentType, Content)
getCartEpubR = do
    html <- getCartHtml
    setHeader "Content-disposition" "attachment; filename=MyDocs.epub"
    return ("application/epub+zip", toContent $ epub html)
