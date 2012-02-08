{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.EditPage
    ( getEditPageR
    , postEditPageR
    , postFileLabelsR
    , routes
    , setCanons
    , getDeletePageR
    , postDeletePageR
    ) where

import Foundation
import qualified Data.Text as T
import FormatHandler
import FileStore
import Data.Conduit (($$), runResourceT)
import Data.Conduit.List (consume, sourceList)
import qualified Data.ByteString as S
import Data.Maybe (isJust, mapMaybe)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.URI.Conduit
import Control.Monad (unless, forM_, filterM)
import Handler.Feed (addFeedItem)
import Text.Hamlet (shamlet)
import Handler.Profile (getLabels)
import Control.Applicative (pure)

checkPerms :: [T.Text] -> Handler ()
checkPerms [] = permissionDenied "Cannot edit page"
checkPerms ("wiki":_) = return ()
checkPerms ("page":_) = do
    Entity _ u <- requireAuth
    unless (userAdmin u) $ permissionDenied "Only admins can edit this page"
checkPerms ("home":user:_) = do
    uid <- requireAuthId
    unless (toPathPiece uid == user) $ permissionDenied "You do not own this page"
checkPerms _ = permissionDenied "Path not understood"

getEditPageR :: [T.Text] -> Handler RepHtml
getEditPageR ts = do
    checkPerms ts
    let ext = snd $ T.breakOnEnd "." $ safeLast "" ts
    Cms { formatHandlers = fhs, fileStore = fs } <- getYesod
    fh <- maybe (invalidArgs ["Invalid file extension: " `T.append` ext]) return $ findHandler ext fhs
    let t = T.intercalate "/" ts
    mecontents <- liftIO $ fsGetFile fs t
    mcontents <- liftIO $
        case mecontents of
            Nothing -> return Nothing
            Just uri -> fmap (Just . decodeUtf8With lenientDecode . S.concat) $ runResourceT $ readURI (fsSM fs) uri $$ consume
    ((res, widget), enctype) <- runFormPost $ fhForm fh mcontents
    case res of
        FormSuccess c -> do
            liftIO $ fsPutFile fs t $ sourceList [encodeUtf8 c]
            runDB $ do
                setCanons t
                addFeedItem "File updated" (RedirectorR t) [] [shamlet|File updated: #{t}|]
            setMessage "File contents updated"
        _ -> return ()
    let toView = isJust mcontents || isSucc res
    labels <- runDB getLabels
    fid <- fmap (either entityKey id) $ runDB $ insertBy $ FileName (T.append "fs:" t) Nothing Nothing
    myLabels <- fmap (map $ fileLabelLabel . entityVal) $ runDB $ selectList [FileLabelFile ==. fid] []
    let isChecked = flip elem myLabels
    defaultLayout $(widgetFile "edit-page")
  where
    isSucc FormSuccess{} = True
    isSucc _ = False
    safeLast t [] = t
    safeLast _ x = last x

routes :: [T.Text] -> [(T.Text, Route Cms)]
routes ("wiki":rest) = [("Wiki page", WikiR $ safeInit rest)]
routes ("home":uid:rest) = [("Home folder", UserFileIntR uid rest)]
routes ["page", _] = [("Homepage", RootR)]
routes ("page":x:xs) = [("Static page", PageR x $ safeInit xs)]
routes _ = []

safeInit :: [a] -> [a]
safeInit [] = []
safeInit x = init x

postEditPageR :: [T.Text] -> Handler RepHtml
postEditPageR ts = do
    checkPerms ts
    toDelete <- runInputPost $ iopt textField "delete"
    case toDelete of
        Just{} -> redirect $ DeletePageR ts
        Nothing -> getEditPageR ts

getDeletePageR, postDeletePageR :: [T.Text] -> Handler RepHtml
getDeletePageR = postDeletePageR
postDeletePageR ts = do
    checkPerms ts
    ((res, widget), enctype) <- runFormPost $ renderDivs $ pure () -- Just getting a nonce...
    mconfirm <- runInputPost $ iopt textField "confirm"
    liftIO $ print (res, mconfirm)
    case (res, mconfirm) of
        (FormSuccess (), Just{}) -> do
            Cms { fileStore = fs } <- getYesod
            let t = T.intercalate "/" ts
            liftIO $ fsDelete fs t
            setMessage "Page deleted"
            runDB $ addFeedItem "Page deleted" (RedirectorR t) [] [shamlet|Page deleted: #{t}|]
            redirect RootR
        _ -> defaultLayout $(widgetFile "delete-page")

setCanons :: FileStorePath -> YesodDB Cms Cms ()
setCanons t = do
    deleteWhere [CanonPathReferer ==. t]
    render <- lift getUrlRenderParams
    let ext = snd $ T.breakOnEnd "." t
    Cms { formatHandlers = fhs, fileStore = fs } <- lift getYesod
    case findHandler ext fhs of
        Nothing -> return ()
        Just fh -> do
            muri <- liftIO $ fsGetFile fs t
            case muri of
                Nothing -> return ()
                Just uri -> do
                    pairs <- liftIO $ fhRefersTo fh (fsSM fs) uri
                    forM_ pairs $ \(referedURI, (route, query)) ->
                        case fsFromURI fs referedURI of
                            Nothing -> return ()
                            Just refered -> do
                                _ <- insert CanonPath
                                    { canonPathReferer = t
                                    , canonPathRefered = refered
                                    , canonPathRedirect = render route query
                                    }
                                return ()

postFileLabelsR :: [T.Text] -> Handler ()
postFileLabelsR ts = do
    checkPerms ts
    let t = T.intercalate "/" ts
    fid <- fmap (either entityKey id) $ runDB $ insertBy $ FileName (T.append "fs:" t) Nothing Nothing
    (posts, _) <- runRequestBody
    let isLabelId = fmap (maybe False (const True)) . get
    lids <- runDB $ filterM isLabelId $ mapMaybe (fromPathPiece . snd) $ filter (\(x, _) -> x == "labels") posts
    runDB $ do
        deleteWhere [FileLabelFile ==. fid]
        mapM_ (insert . FileLabel fid) lids
    setMessage "Labels updated"
    redirect $ EditPageR ts
