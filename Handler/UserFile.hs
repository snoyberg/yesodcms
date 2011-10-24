{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
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

import Foundation
import qualified Data.Text as T
import Data.Monoid (mempty)
import FileStore
import FormatHandler
import Control.Monad (unless, when)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Set as Set
import Data.Maybe (listToMaybe, isJust)
import Handler.EditPage (routes, setCanons)
import Network.HTTP.Types (decodePathSegments)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Map as Map
import Data.Enumerator (($=), enumList)
import qualified Data.Enumerator.List as EL
import Blaze.ByteString.Builder (fromByteString)
import Network.URI.Enumerator (readURI)
import Codec.Archive.Zip
import Control.Spoon (spoon)
import Control.Monad.Trans.Writer (tell, execWriterT)
import qualified Data.ByteString.Lazy as L
import Data.Time
import Network.HTTP.Enumerator
import Data.IORef (writeIORef)
import System.Process hiding (readProcess)
import System.Exit
import Control.Exception hiding (Handler)
import System.IO (hFlush, hClose)
import System.IO.Error
import Control.Concurrent.MVar
import Control.Concurrent
import GHC.IO.Exception (IOErrorType(..))
import Data.List (sort)

getUsersR :: Handler RepHtml
getUsersR = do
    users <- runDB $ fmap (map snd) $ selectList [] [Asc UserHandle]
    defaultLayout $(widgetFile "users")

getUserFileIntR :: T.Text -> [T.Text] -> Handler RepHtml
getUserFileIntR uid' ts = do
    uid <- maybe notFound return $ fromSinglePiece uid'
    u <- runDB $ get404 uid
    gets <- reqGetParams `fmap` getRequest
    redirectParams RedirectPermanent (UserFileR (userHandle u) ts) gets

getUserFileR :: T.Text -> [T.Text] -> Handler RepHtml
getUserFileR user ts = do
    (uid, _) <- runDB $ getBy404 $ UniqueHandle user
    mu <- maybeAuth
    let canWrite = Just uid == fmap fst mu
    let ts' = "home" : toSinglePiece uid : ts
    let t = T.intercalate "/" ts'
    Cms { fileStore = fs, formatHandlers = fhs, rawFiles } <- getYesod
    menum <- liftIO $ fsGetFile fs t
    case menum of
        Nothing -> do
            contents <- liftIO $ fsList fs t
            let folders = sort $ map fst $ filter snd contents
            let files = sort $ map fst $ filter (not . snd) contents
            let link x = UserFileR user $ ts ++ [x]
                editLink x = EditPageR $ ts' ++ [x]
                isEditable x = isJust $ findHandler (snd $ T.breakOnEnd "." x) fhs
            let formats = zip [0 :: Int ..] $ map fhName fhs
            defaultLayout $(widgetFile "user-folder")
        Just enum -> do
            let ext = snd $ T.breakOnEnd "." t
            case Map.lookup (T.toLower ext) $ rawFiles of
                -- FIXME re-enable sendfile optimization
                Just mime -> sendResponse (mime, ContentEnum (readURI (fsSM fs) enum $= EL.map fromByteString))
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
                                        Just (_, u)
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
                        Just (_, canon) -> redirectText RedirectTemporary $ canonPathRedirect canon

blogForm :: Html -> Form Cms Cms (FormResult T.Text, Widget)
blogForm =
    renderTable $ areq (checkM validSlug textField) "Blog slug" Nothing
  where
    validSlug :: T.Text -> GGHandler Cms Cms IO (Either T.Text T.Text)
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

aliasForm :: Maybe T.Text -> Maybe T.Text -> Html -> Form Cms Cms (FormResult (T.Text, T.Text, T.Text), Widget)
aliasForm murl mtitle =
    renderTable $ (,,)
        <$> areq (checkM validSlug textField) "Alias slug" Nothing
        <*> areq urlField "Destination" murl
        <*> areq textField "Title" mtitle
  where
    validSlug :: T.Text -> GGHandler Cms Cms IO (Either T.Text T.Text)
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
    (uid, u) <- requireAuth
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
            redirect RedirectTemporary $ BlogPostR year month slug
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
    (_uid, u) <- requireAuth
    unless (userAdmin u) $ permissionDenied "Only admins can make aliases"
    ((res, widget), _) <- runFormPost $ aliasForm Nothing Nothing
    case res of
        FormSuccess (slug, url, title) -> do
            let a = Alias
                    { aliasSlug = slug
                    , aliasOrig = T.drop 1 $ snd $ T.break (== '/') $ T.drop 2 $ snd $ T.breakOn "//" url
                    , aliasTitle = title
                    }
            as <- runDB $ insert a >> fmap (map snd) (selectList [] [])
            Cms { aliases } <- getYesod
            liftIO $ writeIORef aliases as
            setMessage "Alias created"
            redirectText RedirectTemporary $ T.cons '/' slug
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
    (uid, _) <- runDB $ getBy404 $ UniqueHandle user
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
                Just url ->
                    case parseUrl $ T.unpack url of
                        Nothing -> invalidArgs ["Invalid URL: " `T.append` url]
                        Just req -> do
                            res <- liftIO $ withManager $ httpLbsRedirect req -- FIXME put in some DoS prevention
                            uploadZipLbs uid fs fhs rfs $ responseBody res
        Just fi -> uploadZipLbs uid fs fhs rfs $ fileContent fi
  where
    uploadZipLbs uid fs fhs rfs lbs = do
        -- zip-archive uses async exceptions... oh joy
        -- Use spoon to make sure we don't 500 on bad input
        let archive = toArchive lbs
        case spoon $ length $ zEntries archive of
            Nothing -> do
                setMessage "Invalid ZIP file"
            Just{} -> do
                let toPath e = T.intercalate "/" $ "home" : toSinglePiece uid : ts ++ T.splitOn "/" (T.pack $ eRelativePath e)
                (updated, nu) <- execWriterT $ mapM_ (upload fs rfs fhs toPath) $ zEntries archive
                runDB $ mapM_ setCanons $ Set.toList updated -- FIXME doesn't seem to be working
                setMessage $
                    if Set.null nu
                        then "ZIP file uploaded successfully"
                        else toHtml $ "The following files could not be uploaded: " `T.append` (T.intercalate ", " $ Set.toList nu)
        redirect RedirectTemporary $ UserFileR user ts
    atMay :: Int -> [a] -> Maybe a
    atMay _ [] = Nothing
    atMay 0 (x:_) = Just x
    atMay i (_:xs) = atMay (i - 1) xs

    upload fs rawFiles fhs toPath entry = do
        let t = toPath entry
        let (beforeext, ext) = T.breakOnEnd "." t
        let lbs = fromEntry entry
        let onSucc = tell (Set.singleton t, Set.empty)
        let onErr = tell (Set.empty, Set.singleton $ T.pack $ eRelativePath entry)
        case Map.lookup (T.toLower ext) rawFiles of
            Nothing
                | T.toLower ext `elem` ["eps", "svg"] -> do
                    let t' = beforeext `T.append` "png"
                    lbs' <- liftIO $ epsToPng lbs
                    liftIO $ fsPutFile fs t' $ enumList 8 $ L.toChunks lbs'
                    onSucc
                | otherwise ->
                    case findHandler ext fhs of
                        Nothing -> onErr
                        Just fh ->
                            case fhFilter fh lbs of
                                Just enum -> liftIO $ fsPutFile fs t enum
                                Nothing -> onErr
            Just{} -> do
                liftIO $ fsPutFile fs t $ enumList 8 $ L.toChunks lbs
                onSucc

-- | Use ImageMagick to convert an EPS to a PNG
epsToPng :: L.ByteString -> IO L.ByteString
epsToPng = readProcess "convert" ["-", "png:-"]

readProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> L.ByteString             -- ^ standard input
    -> IO L.ByteString          -- ^ stdout
readProcess cmd args input = do
    (Just inh, Just outh, _, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = Inherit }

    -- fork off a thread to start consuming the output
    output  <- L.hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ evaluate (L.length output) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (L.null input)) $ do L.hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return output
     ExitFailure r -> 
      ioError (mkIOError OtherError ("readProcess: " ++ cmd ++ 
                                     ' ':unwords (map show args) ++ 
                                     " (exit " ++ show r ++ ")")
                                 Nothing Nothing)

getRedirectorR :: T.Text -> Handler ()
getRedirectorR t =
    case routes $ fixExtension $ decodePathSegments $ encodeUtf8 t of
        [] -> notFound
        (_, r):_ -> do
            gets <- reqGetParams `fmap` getRequest
            redirectParams RedirectPermanent r gets

fixExtension :: [T.Text] -> [T.Text]
fixExtension [t]
    | Just t' <- T.stripSuffix ".eps" t = [t' `T.append` ".png"]
    | Just t' <- T.stripSuffix ".svg" t = [t' `T.append` ".png"]
fixExtension [] = []
fixExtension (x:xs) = x : fixExtension xs

getRawR :: T.Text -> Handler RepHtml
getRawR t = do
    Cms { fileStore = fs, formatHandlers = fhs } <- getYesod
    muri <- liftIO $ fsGetFile fs t
    uri <- maybe notFound return muri
    let ext = snd $ T.breakOnEnd "." t
    fh <- maybe notFound return $ findHandler ext fhs
    pc <- widgetToPageContent $ fhFlatWidget fh (fsSM fs) uri
    muid <- maybeAuthId
    showAdd <-
        case muid of
            Nothing -> return False
            Just uid -> runDB $ do
                fid <- getFileNameId t
                x <- getBy $ UniqueCart uid fid
                return $ maybe True (const False) x
    hamletToRepHtml [hamlet|
<div .raw>
    <a href="#" .close>X
    <a .inner-link href=@{RedirectorR t}>
        $if (==) "xml" ext
            Open Manual
        $else
            Open Article
    $if showAdd
        <input .addcart data-action=@{AddCartR t} type=submit value="Add to MyDocs">
    $else
        $maybe _ <- muid
            <a style=display:inline-block;margin-left:2em;color:#900 href=@{CartR}>In MyDocs
^{pageBody pc}
|]
