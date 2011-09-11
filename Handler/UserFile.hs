{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.UserFile
    ( getUsersR
    , getUserFileIntR
    , getUserFileR
    , postUserFileR
    ) where

import Foundation
import qualified Data.Text as T
import Data.Monoid (mempty)
import FileStore
import FormatHandler
import Control.Monad (unless)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)

getUsersR :: Handler RepHtml
getUsersR = do
    users <- runDB $ fmap (map $ userHandle . snd) $ selectList [] [Asc UserHandle]
    defaultLayout $(widgetFile "users")

getUserFileIntR :: T.Text -> [T.Text] -> Handler RepHtml
getUserFileIntR uid' ts = do
    uid <- maybe notFound return $ fromSinglePiece uid'
    u <- runDB $ get404 uid
    redirect RedirectPermanent $ UserFileR (userHandle u) ts

getUserFileR :: T.Text -> [T.Text] -> Handler RepHtml
getUserFileR user ts = do
    (uid, _) <- runDB $ getBy404 $ UniqueHandle user
    muid <- maybeAuthId
    let canWrite = Just uid == muid
    let t = T.intercalate "/" $ "home" : toSinglePiece uid : ts
    Cms { fileStore = fs, formatHandlers = fhs } <- getYesod
    menum <- liftIO $ fsGetFile fs t
    case menum of
        Nothing -> do
            contents <- liftIO $ fsList fs t
            let folders = map fst $ filter snd contents
            let files = map fst $ filter (not . snd) contents
            let link x = UserFileR user $ ts ++ [x]
            let formats = zip [0 :: Int ..] $ map fhName fhs
            defaultLayout $(widgetFile "user-folder")
        Just enum -> do
            let ext = snd $ T.breakOnEnd "." t
            fh <- maybe notFound return $ findHandler ext fhs
            defaultLayout $ fhWidget fh (fsSM fs) enum

postUserFileR :: T.Text -> [T.Text] -> Handler ()
postUserFileR user ts = do
    (uid, _) <- runDB $ getBy404 $ UniqueHandle user
    muid <- maybeAuthId
    unless (Just uid == muid) $ permissionDenied "Only the owner can edit these files"
    Cms { fileStore = fs, formatHandlers = fhs } <- getYesod
    mfolder <- runInputPost $ iopt textField "folder"
    case mfolder of
        Just folder -> do
            let ts' = ts ++ [folder]
                t = T.intercalate "/" $ "home" : toSinglePiece uid : ts'
            liftIO $ fsMkdir fs t
            setMessage "Folder created"
            redirect RedirectTemporary $ UserFileR user ts'
        Nothing -> do
            (file, formatNum) <- runInputPost $ (,) <$> ireq textField "file" <*> ireq intField "format"
            format <- maybe (invalidArgs ["Incorrect format"]) return $ atMay formatNum fhs
            ext <- maybe (error "All FormatHandlers need at least one extension") return
                 $ listToMaybe $ Set.toList $ fhExts format
            redirect RedirectTemporary $ EditPageR $ "home" : toSinglePiece uid : ts ++ [T.concat [file, ".", ext]]
  where
    atMay :: Int -> [a] -> Maybe a
    atMay _ [] = Nothing
    atMay 0 (x:_) = Just x
    atMay i (_:xs) = atMay (i - 1) xs
