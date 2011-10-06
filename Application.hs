{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withCms
    , withDevelAppPort
    ) where

import Foundation
import Settings
import Settings.StaticFiles (static)
import Yesod.Auth
import Yesod.Logger (Logger)
import Yesod.Default.Config
import Yesod.Default.Main
import Database.Persist.GenericSql
import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic, toDyn)
import FormatHandler.Html
import FormatHandler.Text
import FormatHandler.Markdown
import FormatHandler.DITA
import FormatHandler.Video
import FileStore
import Network.URI.Enumerator
import qualified Network.URI.Enumerator.File as File
import DITA.Util.ClassMap (loadClassMap)
import Data.DTD.Cache
import DITA.Types (hrefFile, NavId (..), FileId (..))
import qualified DITA.Types as D
import qualified Data.Map as Map
import Data.IORef
import qualified Network.Wai as W
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (join)
import qualified Network.HTTP.Types as H
import qualified Data.Text as T
import Blaze.ByteString.Builder (toByteString)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

#ifndef WINDOWS
import qualified System.Posix.Signals as Signal
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
#endif

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Wiki
import Handler.EditPage
import Handler.UserFile
import Handler.Profile
import Handler.Page
import Handler.Comments
import Handler.Feed
import Handler.Blog
import Handler.Search
import Handler.Cart
import Handler.Upload

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Cms" resourcesCms

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withCms :: AppConfig DefaultEnv -> Logger -> (Application -> IO a) -> IO ()
withCms conf logger f = do
    s <- static Settings.staticDir
    Settings.withConnectionPool conf $ \p -> do
        runConnectionPool (runMigration migrateAll) p
        cm <- File.decodeString "dita/classmap.css"
        let sm = toSchemeMap [File.fileScheme]
        classmap <- loadClassMap sm cm
        cache <- newDTDCacheFile "dita/catalog-dita.xml"
        let raw = Map.fromList
                [ ("png", "image/png")
                , ("gif", "image/gif")
                , ("jpg", "image/jpeg")
                , ("jpeg", "image/jpeg")
                ]
        idocCache <- newIORef Map.empty
        ialiases <- runConnectionPool (selectList [] []) p >>= newIORef . map snd
        let renderHref = flip (yesodRender h) [] . RedirectorR . uriPath . hrefFile
            h = Cms conf logger s p
                    [ textFormatHandler
                    , markdownFormatHandler
                    , htmlFormatHandler
                    , videoFormatHandler
                    , ditaFormatHandler renderHref cache classmap (loadFileId p) addToCartURI
                    , ditamapFormatHandler renderHref cache classmap (loadFileId p) idocCache toDocRoute toNavRoute addToCartURI
                    ] (simpleFileStore "data") raw ialiases
#ifdef WINDOWS
        toWaiApp h >>= f . book ialiases >> return ()
#else
        tid <- forkIO $ toWaiApp h >>= f . book ialiases >> return ()
        flag <- newEmptyMVar
        _ <- Signal.installHandler Signal.sigINT (Signal.CatchOnce $ do
            putStrLn "Caught an interrupt"
            killThread tid
            putMVar flag ()) Nothing
        takeMVar flag
#endif

book :: IORef [Alias] -> W.Middleware
book ialiases app req = do
    liftIO (readIORef ialiases) >>= go
  where
    go [] = app req
    go (a:as) =
        case W.pathInfo req of
            [x] | x == aliasSlug a -> app req
                { W.pathInfo = pieces
                }
            [x, nav] | x == aliasSlug a -> app req
                { W.pathInfo = pieces
                , W.queryString = ("nav", Just $ encodeUtf8 nav) : noNav
                }
            x | x == pieces ->
                case join $ lookup "nav" $ W.queryString req of
                    Nothing -> redir [aliasSlug a] $ W.queryString req
                    Just nav -> redir [aliasSlug a, decodeUtf8With lenientDecode nav] noNav
            _ -> go as
      where
        pieces = H.decodePathSegments $ encodeUtf8 $ aliasOrig a

    redir ps qs =
        return $ W.responseLBS H.status301 [("Location", toByteString path)] ""
      where
        path = H.encodePath ps qs
    noNav = filter (\(x, _) -> x /= "nav") $ W.queryString req

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withCms

toDocRoute :: URI -> CmsRoute
toDocRoute uri = RedirectorR $ uriPath uri

toNavRoute :: URI -> NavId -> D.FileId -> D.TopicId -> (CmsRoute, [(T.Text, T.Text)])
toNavRoute uri (NavId nid) (D.FileId fid) (D.TopicId tid) = (RedirectorR (uriPath uri), [("nav", nid), ("topic", T.concat [fid, "-", tid])])

loadFileId :: Settings.ConnectionPool -> URI -> IO FileId
loadFileId p uri = flip Settings.runConnectionPool p $ do
    let str = T.pack $ show $ toNetworkURI uri
    fid <- fmap (either fst id) $ insertBy $ FileName str Nothing Nothing
    return $ FileId $ "file" `T.append` toSinglePiece fid
