{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.EditPage
    ( getEditPageR
    , postEditPageR
    , routes
    , setCanons
    ) where

import Foundation
import qualified Data.Text as T
import FormatHandler
import FileStore
import Data.Enumerator (($$), run_, enumList)
import Data.Enumerator.List (consume)
import qualified Data.ByteString as S
import Data.Maybe (isJust)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.URI.Enumerator
import Control.Monad (unless, forM_)

checkPerms :: [T.Text] -> Handler ()
checkPerms [] = permissionDenied "Cannot edit page"
checkPerms ("wiki":_) = return ()
checkPerms ("page":_) = do
    (_, u) <- requireAuth
    unless (userAdmin u) $ permissionDenied "Only admins can edit this page"
checkPerms ("home":user:_) = do
    uid <- requireAuthId
    unless (toSinglePiece uid == user) $ permissionDenied "You do not own this page"
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
            Just uri -> fmap (Just . decodeUtf8With lenientDecode . S.concat) $ run_ $ readURI (fsSM fs) uri $$ consume
    ((res, widget), enctype) <- runFormPost $ fhForm fh mcontents
    case res of
        FormSuccess c -> do
            liftIO $ fsPutFile fs t $ enumList 1 [encodeUtf8 c]
            runDB $ setCanons t
            setMessage "File contents updated"
        _ -> return ()
    let toView = isJust mcontents || isSucc res
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
        Just{} -> do
            -- FIXME Delete confirmation
            Cms { fileStore = fs } <- getYesod
            let t = T.intercalate "/" ts
            liftIO $ fsDelete fs t
            setMessage "Page deleted"
            redirect RedirectTemporary RootR
        Nothing -> getEditPageR ts

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
