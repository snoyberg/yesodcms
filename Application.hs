module Application
    ( getApplication
    , getApplicationDev
    ) where

import Import
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Database.Persist.GenericSql
import Data.ByteString (ByteString)
import FormatHandler.Html
import FormatHandler.Text
import FormatHandler.Markdown
import FileStore
import qualified Data.Map as Map
import Data.IORef
import qualified Network.Wai as W
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (join)
import qualified Network.HTTP.Types as H
import Blaze.ByteString.Builder (toByteString)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Conduit (newManager, def)
import qualified Settings

import qualified Database.Persist.Store

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

getApplication :: AppConfig DefaultEnv Extra -> IO Application
getApplication conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
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
    ialiases <- Database.Persist.Store.runPool dbconf (selectList [] []) p >>= newIORef . map entityVal
    let renderHref = flip (yesodRender h) [] . RedirectorR . uriPath . hrefFile
        h = Cms conf logger s p
                [ markdownFormatHandler
                , htmlFormatHandler
                , lhaskellFormatHandler
                , textFormatHandler
                , ditaFormatHandler renderHref cache classmap (loadFileId dbconf p)
                , ditamapFormatHandler renderHref cache classmap (loadFileId dbconf p) idocCache toDocRoute toNavRoute
                ] (simpleFileStore "data") raw ialiases dbconf manager
    app <- toWaiApp h
    return $ book ialiases app

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
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader getApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

toDocRoute :: URI -> CmsRoute
toDocRoute uri = RedirectorR $ uriPath uri

toNavRoute :: URI -> NavId -> D.FileId -> D.TopicId -> (CmsRoute, [(T.Text, T.Text)])
toNavRoute uri (NavId nid) (D.FileId fid) (D.TopicId tid) = (RedirectorR (uriPath uri), [("nav", nid), ("topic", T.concat [fid, "-", tid])])

loadFileId :: PersistConfig -> Database.Persist.Store.PersistConfigPool PersistConfig -> URI -> IO FileId
loadFileId dbconf p uri = flip (Database.Persist.Store.runPool dbconf) p $ do
    let str = T.pack $ show $ toNetworkURI uri
    fid <- fmap (either entityKey id) $ insertBy $ FileName str Nothing Nothing
    return $ FileId $ "file" `T.append` toPathPiece fid
