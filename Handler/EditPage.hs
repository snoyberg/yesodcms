{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.EditPage
    ( getEditPageR
    , postEditPageR
    , postFileLabelsR
    , routes
    , setCanons
    , getDeletePageR
    , postDeletePageR
    , getLabelListR
    , postLabelListR
    ) where

import Foundation
import qualified Data.Text as T
import FormatHandler
import FileStore
import Data.Enumerator (($$), run_, enumList)
import Data.Enumerator.List (consume)
import qualified Data.ByteString as S
import Data.Maybe (isJust, mapMaybe, fromMaybe)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.URI.Enumerator
import Control.Monad (unless, forM_, filterM)
import Handler.Feed (addFeedItem)
import Text.Hamlet (shamlet)
import Handler.Profile (getLabels)
import Control.Applicative (pure)
import Text.CSV
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map as Map
import Database.Persist.Base (PersistFilter (BackendSpecificFilter))

checkPerms :: Bool -> [T.Text] -> Handler ()
checkPerms isDelete ts = do
    u <- requireAuth
    checkPerms' isDelete u ts

checkPerms' :: Monad m => Bool -> (UserId, User) -> [T.Text] -> GGHandler sub Cms m ()
checkPerms' _ _ [] = permissionDenied "Cannot edit page"
checkPerms' _isDelete (_, _u) ("wiki":_) =
    return ()
    --when (isDelete && not (userAdmin u)) $ permissionDenied "Only admins can delete this page"
checkPerms' _ (_, u) ("page":_) =
    unless (userAdmin u) $ permissionDenied "Only admins can edit this page"
checkPerms' _ (uid, _) ("home":user:_) = do
    unless (toSinglePiece uid == user) $ permissionDenied "You do not own this page"
checkPerms' _ _ _ = permissionDenied "Path not understood"

checkURIPerms :: Monad m => Bool -> (UserId, User) -> URI -> GGHandler sub Cms m ()
checkURIPerms isDelete u uri = checkPerms' isDelete u $ T.splitOn "/" $ uriPath uri

getEditPageR :: [T.Text] -> Handler RepHtml
getEditPageR ts = do
    checkPerms False ts
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
            runDB $ do
                setCanons t
                addFeedItem "File updated" (RedirectorR t) [] [shamlet|File updated: #{t}|]
            setMessage "File contents updated"
        _ -> return ()
    let toView = isJust mcontents || isSucc res
    labels <- runDB getLabels
    fid <- runDB $ getFileNameId t
    myLabels <- fmap (map $ fileLabelLabel . snd) $ runDB $ selectList [FileLabelFile ==. fid] []
    let isChecked = flip elem myLabels
    isLabels' <- runInputGet $ iopt boolField "labels"
    let isLabels = fromMaybe False isLabels'
    mu <- maybeAuth
    let isAdmin = fmap (userAdmin . snd) mu == Just True
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
    checkPerms False ts
    toDelete <- runInputPost $ iopt textField "delete"
    case toDelete of
        Just{} -> redirect RedirectTemporary $ DeletePageR ts
        Nothing -> getEditPageR ts

getDeletePageR, postDeletePageR :: [T.Text] -> Handler RepHtml
getDeletePageR = postDeletePageR
postDeletePageR ts = do
    checkPerms True ts
    ((res, widget), enctype) <- runFormPost $ renderDivs $ pure () -- Just getting a nonce...
    mconfirm <- runInputPost $ iopt textField "confirm"
    case (res, mconfirm) of
        (FormSuccess (), Just{}) -> do
            Cms { fileStore = fs } <- getYesod
            let t = T.intercalate "/" ts
            liftIO $ fsDelete fs t
            runDB $ do
                fid <- getFileNameId t
                deleteWhere [ArticleFile ==. fid]
            setMessage "Page deleted"
            runDB $ addFeedItem "Page deleted" (RedirectorR t) [] [shamlet|Page deleted: #{t}|]
            redirect RedirectTemporary RootR
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
    checkPerms False ts
    let t = T.intercalate "/" ts
    fid <- runDB $ getFileNameId t
    (posts, _) <- runRequestBody
    let isLabelId = fmap (maybe False (const True)) . get
    lids <- runDB $ filterM isLabelId $ mapMaybe (fromSinglePiece . snd) $ filter (\(x, _) -> x == "labels") posts
    runDB $ do
        deleteWhere [FileLabelFile ==. fid]
        mapM_ (insert . FileLabel fid) lids
    setMessage "Labels updated"
    redirect RedirectTemporary $ EditPageR ts

getLabelListR :: [T.Text] -> Handler (ContentType, Content)
getLabelListR ts = do
    checkPerms False ts
    let t = T.intercalate "/" ts
    let ext = snd $ T.breakOnEnd "." t
    Cms { formatHandlers = fhs, fileStore = fs } <- getYesod
    fh <- maybe notFound return $ findHandler ext fhs
    uri <- liftIO (fsGetFile fs t) >>= maybe notFound return
    rows <- liftIO $ fmap (map fst) (fhRefersTo fh (fsSM fs) uri) >>= mapM (\u -> do
        let t' = uriPath u
        title <- fileTitle' fs fhs t'
        return [show $ toNetworkURI u, T.unpack title]
        )
    setHeader "Content-disposition" $ encodeUtf8 $ "attachment; filename=" `T.append` t `T.append` ".csv"
    return ("text/csv", toContent $ printCSV $ ["URI", "Title"] : rows)

postLabelListR :: [T.Text] -> Handler ()
postLabelListR ts = do
    user <- requireAuth
    (_, files) <- runRequestBody
    csvFile <- maybe (invalidArgs ["No file uploaded" :: T.Text]) return $ lookup "csv" files
    records <- either (invalidArgs . return . T.pack . show) return $ parseCSV "" $ L8.unpack $ fileContent csvFile
    (frow, rest) <-
        case records of
            [] -> invalidArgs ["No rows"]
            (x:xs) -> return (x, xs)
    let toMap = Map.fromList . zip frow
    let maps = map toMap rest
    runDB $ forM_ maps $ \m -> do
        uriT' <- maybe (lift $ invalidArgs ["No URI"]) (return . T.pack) $ Map.lookup "URI" m
        let escapeSpace ' ' = "%20"
            escapeSpace c = T.singleton c
        let uriT = T.concatMap escapeSpace uriT'
        case parseURI uriT of
            Nothing -> return ()
            Just uri -> do
                lift $ checkURIPerms False user uri
                forM_ (Map.toList m) $ \(k, v) -> do
                    mg <- getBy $ UniqueGroup $ T.pack k
                    fid <- getFileNameIdURI uri
                    case mg of
                        Nothing -> return ()
                        Just (gid, _) -> do
                            ls <- selectList [LabelGroup ==. gid, LabelName `like` addPercent (T.pack v)] []
                            forM_ ls $ \(lid, _) -> insertBy (FileLabel fid lid) >> return ()
    setMessage "Labels applied"
    redirect RedirectTemporary $ EditPageR ts

like :: PersistField typ => EntityField v typ -> typ -> Filter v
like f a = Filter f (Left a) (BackendSpecificFilter " LIKE ")

addPercent :: T.Text -> T.Text
addPercent t
    | T.null t = t
    | otherwise = T.snoc t '%'
