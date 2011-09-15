{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, NamedFieldPuns #-}
module Handler.UserFile
    ( getUsersR
    , getUserFileIntR
    , getUserFileR
    , postUserFileR
    , getRedirectorR
    , postCreateBlogR
    ) where

import Foundation
import qualified Data.Text as T
import Data.Monoid (mempty)
import FileStore
import FormatHandler
import Control.Monad (unless)
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
import Yesod.Goodies.Gravatar
import Data.Time

getUsersR :: Handler RepHtml
getUsersR = do
    users <- runDB $ fmap (map snd) $ selectList [] [Asc UserHandle]
    defaultLayout $(widgetFile "users")
  where
    opts = defaultOptions
        { gSize = Just $ Size 80
        , gDefault = Just Identicon
        }

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
                Just mime -> sendResponse (mime, ContentEnum (readURI (fsSM fs) enum $= EL.map fromByteString))
                Nothing -> do
                    -- Check for a canonical path and redirect if present
                    mcanon <- runDB $ selectFirst [CanonPathRefered ==. t] []
                    case mcanon of
                        Nothing -> do
                            fh <- maybe notFound return $ findHandler ext fhs
                            defaultLayout $ do
                                fhWidget fh (fsSM fs) enum
                                mblogPost <-
                                    case mu of
                                        Just (_, u)
                                            | userAdmin u -> fmap Just $ do
                                                let url = CreateBlogR t
                                                ((_, widget), _) <- lift $ runFormPost blogForm
                                                return (url, widget)
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

postUserFileR :: T.Text -> [T.Text] -> Handler ()
postUserFileR user ts = do
    (uid, _) <- runDB $ getBy404 $ UniqueHandle user
    muid <- maybeAuthId
    unless (Just uid == muid) $ permissionDenied "Only the owner can edit these files"
    Cms { fileStore = fs, formatHandlers = fhs, rawFiles } <- getYesod
    (_, files) <- runRequestBody
    case lookup "zip" files of
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
        Just fi -> do
            -- zip-archive uses async exceptions... oh joy
            -- Use spoon to make sure we don't 500 on bad input
            let archive = toArchive $ fileContent fi
            case spoon $ length $ zEntries archive of
                Nothing -> do
                    setMessage "Invalid ZIP file"
                Just{} -> do
                    let toPath e = T.intercalate "/" $ "home" : toSinglePiece uid : ts ++ T.splitOn "/" (T.pack $ eRelativePath e)
                    (updated, nu) <- execWriterT $ mapM_ (upload fs rawFiles fhs toPath) $ zEntries archive
                    runDB $ mapM_ setCanons $ Set.toList updated
                    setMessage $
                        if Set.null nu
                            then "ZIP file uploaded successfully"
                            else toHtml $ "The following files could not be uploaded: " `T.append` (T.intercalate ", " $ Set.toList nu)
            redirect RedirectTemporary $ UserFileR user ts
  where
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
        case Map.lookup ext rawFiles of
            Nothing ->
                case findHandler ext fhs of
                    Nothing -> onErr
                    Just fh ->
                        case fhFilter fh lbs of
                            Just enum -> liftIO $ fsPutFile fs t enum
                            Nothing -> onErr
            Just{} -> do
                liftIO $ fsPutFile fs t $ enumList 8 $ L.toChunks lbs
                onSucc

getRedirectorR :: T.Text -> Handler ()
getRedirectorR t =
    case routes $ decodePathSegments $ encodeUtf8 t of
        [] -> notFound
        (_, r):_ -> do
            gets <- reqGetParams `fmap` getRequest
            redirectParams RedirectPermanent r gets
