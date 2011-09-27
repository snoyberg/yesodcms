{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Cart
    ( getCartR
    , postAddCartR
    , postUpCartR
    , postDownCartR
    , postDeleteCartR
    ) where

import Foundation
import qualified Data.Text as T
import Handler.EditPage (getFileNameId)
import Control.Monad (unless)

getCartR :: Handler RepHtml
getCartR = do
    uid <- requireAuthId
    Cms { formatHandlers = fhs, fileStore = fs } <- getYesod
    carts <- runDB $ selectList [CartUser ==. uid] [Asc CartPriority] >>= mapM (\(cid, c) -> do
        file <- get404 $ cartFile c
        let t = T.drop 3 $ fileNameUri file
        title <-
            case fileNameTitle file of
                Just t' -> return t'
                Nothing -> fileTitle' fs fhs t
        return (cid, (RedirectorR t, title))
        )
    defaultLayout $(widgetFile "cart")

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
