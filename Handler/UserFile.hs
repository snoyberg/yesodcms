{-# LANGUAGE NamedFieldPuns #-}
module Handler.UserFile
    ( getUsersR
    , getUserFileIntR
    , getUserFileR
    , postUserFileR
    , getRedirectorR
    , getRawR
    , postCreateBlogR
    , postCreateAliasR
    ) where

import Import
import qualified Data.Text as T
import FileStore
import FormatHandler
import Control.Monad (unless)
import qualified Data.Set as Set
import Data.Maybe (listToMaybe, isJust)
import Handler.EditPage (routes, setCanons)
import Network.HTTP.Types (status301, decodePathSegments)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Map as Map
import Data.Conduit (($=), ($$), Flush (Chunk))
import qualified Data.Conduit.List as CL
import Blaze.ByteString.Builder (fromByteString)
import Network.URI.Conduit (readURI)
import Codec.Archive.Zip
import Control.Spoon (spoon)
import Control.Monad.Trans.Writer (tell, execWriterT)
import qualified Data.ByteString.Lazy as L
import Network.Gravatar
import Network.HTTP.Conduit
import Data.IORef (writeIORef)
import Data.Time (getCurrentTime, toGregorian, utctDay)

getUsersR :: Handler RepHtml
getUsersR = do
    users <- runDB $ fmap (map entityVal) $ selectList [] [Asc UserHandle]
    defaultLayout $(widgetFile "users")
  where
    opts = def
        { gSize = Just $ Size 80
        , gDefault = Just Identicon
        }

getUserFileIntR :: T.Text -> [T.Text] -> Handler RepHtml
getUserFileIntR uid' ts = do
    uid <- maybe notFound return $ fromPathPiece uid'
    u <- runDB $ get404 uid
    gets <- reqGetParams `fmap` getRequest
    redirectWith status301 (UserFileR (userHandle u) ts, gets)

getUserFileR :: T.Text -> [T.Text] -> Handler RepHtml
getUserFileR user ts = do
    Entity uid _ <- runDB $ getBy404 $ UniqueHandle user
    mu <- maybeAuth
    let canWrite = Just uid == fmap entityKey mu
    let ts' = "home" : toPathPiece uid : ts
    let t = T.intercalate "/" ts'
    Cms { fileStore = fs, formatHandlers = fhs, rawFiles } <- getYesod
    menum <- liftIO $ fsGetFile fs t
    case menum of
        Nothing -> do
            contents <- liftIO $ fsList fs t
            let folders = map fst $ filter snd contents
            let files = map fst $ filter (not . snd) contents
            let link x = UserFileR user $ ts ++ [x]
                editLink x = EditPageR $ ts' ++ [x]
                isEditable x = isJust $ findHandler (snd $ T.breakOnEnd "." x) fhs
            let formats = zip [0 :: Int ..] $ map fhName fhs
            defaultLayout $(widgetFile "user-folder")
        Just enum -> do
            let ext = snd $ T.breakOnEnd "." t
            case Map.lookup (T.toLower ext) $ rawFiles of
                -- FIXME re-enable sendfile optimization
                Just mime -> sendResponse (mime, ContentSource (readURI (fsSM fs) enum $= CL.map (Chunk . fromByteString)))
                Nothing -> do
                    -- Check for a canonical path and redirect if present
                    mcanon <- runDB $ selectFirst [CanonPathRefered ==. t] []
                    case mcanon of
                        Nothing -> do
                            fh <- maybe notFound return $ findHandler ext fhs
                            parents' <- fhExtraParents fh (fsSM fs) enum
                            defaultLayoutExtraParents parents' $ do
                                fhWidget fh (fsSM fs) enum
                                mblogPost <-
                                    case mu of
                                        Just (Entity _ u)
                                            | userAdmin u -> fmap Just $ do
                                                let url = CreateBlogR t
                                                ((_, widget), _) <- lift $ runFormPost blogForm
                                                Just cr <- lift getCurrentRoute
                                                tm <- lift getRouteToMaster
                                                r <- lift getUrlRender
                                                let url2 = CreateAliasR
                                                title <- lift $ fileTitle t
                                                ((_, widget2), _) <- lift $ runFormPost $ aliasForm (Just $ r $ tm cr) (Just title)
                                                return ((url, widget), (url2, widget2))
                                        _ -> return Nothing
                                $(widgetFile "user-file-edit")
                        Just (Entity _ canon) -> redirect $ canonPathRedirect canon

blogForm :: Form T.Text
blogForm =
    renderTable $ areq (checkM validSlug textField) "Blog slug" Nothing
  where
    validSlug :: T.Text -> Handler (Either T.Text T.Text)
    validSlug slug'
        | T.any invalidChar slug' = return $ Left "Slug must be lowercase letters, numbers and hyphens"
        | otherwise = do
            let slug = BlogSlugT slug'
            (year, month) <- liftIO currYearMonth
            x <- runDB $ getBy $ UniqueBlog year month slug
            case x of
                Nothing -> return $ Right slug'
                Just{} -> return $ Left "Slug already in use, please try again"
    invalidChar c
        | 'a' <= c && c <= 'z' = False
        | '0' <= c && c <= '9' = False
        | c == '-' = False
        | otherwise = True

aliasForm :: Maybe T.Text -> Maybe T.Text -> Form (T.Text, T.Text, T.Text)
aliasForm murl mtitle =
    renderTable $ (,,)
        <$> areq (checkM validSlug textField) "Alias slug" Nothing
        <*> areq urlField "Destination" murl
        <*> areq textField "Title" mtitle
  where
    validSlug :: T.Text -> Handler (Either T.Text T.Text)
    validSlug slug
        | T.any invalidChar slug = return $ Left "Slug must be lowercase letters, numbers and hyphens"
        | otherwise = do
            x <- runDB $ getBy $ UniqueAlias slug
            case x of
                Nothing -> return $ Right slug
                Just{} -> return $ Left "Slug already in use, please try again"
    invalidChar c
        | 'a' <= c && c <= 'z' = False
        | '0' <= c && c <= '9' = False
        | c == '-' = False
        | otherwise = True

currYearMonth :: IO (Int, Month)
currYearMonth = do
    now <- getCurrentTime
    let (year, month, _) = toGregorian $ utctDay now
    return (fromInteger year, Month month)

postCreateBlogR :: T.Text -> Handler RepHtml
postCreateBlogR t = do
    Entity uid u <- requireAuth
    unless (userAdmin u) $ permissionDenied "Only admins can make blog posts"
    ((res, widget), _) <- runFormPost blogForm
    case res of
        FormSuccess slug' -> do
            let slug = BlogSlugT slug'
            now <- liftIO getCurrentTime
            (year, month) <- liftIO $ currYearMonth
            title <- fileTitle t
            _ <- runDB $ insert $ Blog now t slug year month uid title
            setMessage "Blog post created"
            redirect $ BlogPostR year month slug
        _ -> defaultLayout [whamlet|
<form method=post>
    <table>
        ^{widget}
        <tr>
            <td colspan=3>
                <input type=submit value="Create blog post">
|]

postCreateAliasR :: Handler RepHtml
postCreateAliasR = do
    Entity _uid u <- requireAuth
    unless (userAdmin u) $ permissionDenied "Only admins can make aliases"
    ((res, widget), _) <- runFormPost $ aliasForm Nothing Nothing
    case res of
        FormSuccess (slug, url, title) -> do
            let a = Alias
                    { aliasSlug = slug
                    , aliasOrig = T.drop 1 $ snd $ T.break (== '/') $ T.drop 2 $ snd $ T.breakOn "//" url
                    , aliasTitle = title
                    }
            as <- runDB $ insert a >> fmap (map entityVal) (selectList [] [])
            Cms { aliases } <- getYesod
            liftIO $ writeIORef aliases as
            setMessage "Alias created"
            redirect $ T.cons '/' slug
        _ -> defaultLayout [whamlet|
<form method=post>
    <table>
        ^{widget}
        <tr>
            <td colspan=3>
                <input type=submit value="Create alias">
|]

postUserFileR :: T.Text -> [T.Text] -> Handler ()
postUserFileR user ts = do
    Entity uid _ <- runDB $ getBy404 $ UniqueHandle user
    muid <- maybeAuthId
    unless (Just uid == muid) $ permissionDenied "Only the owner can edit these files"
    Cms { fileStore = fs, formatHandlers = fhs, rawFiles = rfs } <- getYesod
    (posts, files) <- runRequestBody
    case lookup "zip" files of
        Nothing -> do
            case lookup "url" posts of
                Nothing -> do
                    mfolder <- runInputPost $ iopt textField "folder"
                    case mfolder of
                        Just folder -> do
                            let ts' = ts ++ [folder]
                                t = T.intercalate "/" $ "home" : toPathPiece uid : ts'
                            liftIO $ fsMkdir fs t
                            setMessage "Folder created"
                            redirect $ UserFileR user ts'
                        Nothing -> do
                            (file, formatNum) <- runInputPost $ (,) <$> ireq textField "file" <*> ireq intField "format"
                            format <- maybe (invalidArgs ["Incorrect format"]) return $ atMay formatNum fhs
                            ext <- maybe (error "All FormatHandlers need at least one extension") return
                                 $ listToMaybe $ Set.toList $ fhExts format
                            redirect $ EditPageR $ "home" : toPathPiece uid : ts ++ [T.concat [file, ".", ext]]
                Just url ->
                    case parseUrl $ T.unpack url of
                        Nothing -> invalidArgs ["Invalid URL: " `T.append` url]
                        Just req -> do
                            y <- getYesod
                            res <- lift $ httpLbs req $ httpManager y -- FIXME put in some DoS prevention
                            uploadZipLbs uid fs fhs rfs $ responseBody res
        Just fi -> do
                  fc <- lift $ fileContent fi
                  uploadZipLbs uid fs fhs rfs fc
  where
    uploadZipLbs uid fs fhs rfs lbs = do
        -- zip-archive uses async exceptions... oh joy
        -- Use spoon to make sure we don't 500 on bad input
        let archive = toArchive lbs
        case spoon $ length $ zEntries archive of
            Nothing -> do
                setMessage "Invalid ZIP file"
            Just{} -> do
                let toPath e = T.intercalate "/" $ "home" : toPathPiece uid : ts ++ T.splitOn "/" (T.pack $ eRelativePath e)
                (updated, nu) <- execWriterT $ mapM_ (upload fs rfs fhs toPath) $ zEntries archive
                runDB $ mapM_ setCanons $ Set.toList updated -- FIXME doesn't seem to be working
                setMessage $
                    if Set.null nu
                        then "ZIP file uploaded successfully"
                        else toHtml $ "The following files could not be uploaded: " `T.append` (T.intercalate ", " $ Set.toList nu)
        redirect $ UserFileR user ts
    atMay :: Int -> [a] -> Maybe a
    atMay _ [] = Nothing
    atMay 0 (x:_) = Just x
    atMay i (_:xs) = atMay (i - 1) xs

    upload fs rawFiles fhs toPath entry = do
        let t = toPath entry
        let ext = snd $ T.breakOnEnd "." t
        let lbs = fromEntry entry
        let onSucc = tell (Set.singleton t, Set.empty)
        let onErr = tell (Set.empty, Set.singleton $ T.pack $ eRelativePath entry)
        case Map.lookup (T.toLower ext) rawFiles of
            Nothing ->
                case findHandler ext fhs of
                    Nothing -> onErr
                    Just fh ->
                        case fhFilter fh lbs of
                            Just enum -> liftIO $ fsPutFile fs t enum
                            Nothing -> onErr
            Just{} -> do
                liftIO $ fsPutFile fs t $ CL.sourceList $ L.toChunks lbs
                onSucc
    fileContent f = L.fromChunks <$> (fileSource f $$ CL.consume)



getRedirectorR :: T.Text -> Handler ()
getRedirectorR t =
    case routes $ decodePathSegments $ encodeUtf8 t of
        [] -> notFound
        (_, r):_ -> do
            gets <- reqGetParams `fmap` getRequest
            redirectWith status301 (r, gets)

getRawR :: T.Text -> Handler RepHtml
getRawR t = do
    Cms { fileStore = fs, formatHandlers = fhs } <- getYesod
    muri <- liftIO $ fsGetFile fs t
    uri <- maybe notFound return muri
    let ext = snd $ T.breakOnEnd "." t
    fh <- maybe notFound return $ findHandler ext fhs
    pc <- widgetToPageContent $ fhFlatWidget fh (fsSM fs) uri
    hamletToRepHtml [hamlet|
<a .inner-link href=@{RedirectorR t}>Open
^{pageBody pc}
|]
