module Foundation
    ( Cms (..)
    , CmsRoute
    , resourcesCms
    , Handler
    , Widget
    , maybeAuthId
    , maybeAuth
    , requireAuthId
    , requireAuth
    , module Yesod
    , module Settings
    , module Model
    , Route (..)
    , fileTitle
    , fileTitle'
    , defaultLayoutExtraParents
    , Form
    ) where

import Yesod
import Yesod.Form.Jquery
import Prelude
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import qualified Settings
import Settings.Development (development)
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import qualified Data.Text as T
import Web.ClientSession (getKey)
import Data.Text (Text)
import Data.Monoid (mempty)
import FormatHandler
import FormatHandler.Html
import FileStore
import Data.Map (Map)
import Yesod.AtomFeed
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef)
import System.Locale (defaultTimeLocale)
import Data.Time (formatTime)
import qualified Database.Persist.Store
import Text.Hamlet (hamletFile)
import Network.HTTP.Conduit (Manager)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Cms = Cms
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , formatHandlers :: [FormatHandler Cms]
    , fileStore :: FileStore
    , rawFiles :: Map T.Text ContentType
    , aliases :: IORef [Alias]
    , persistConfig :: Settings.PersistConfig
    , httpManager :: Manager
    }

mkMessage "Cms" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype CmsRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Cms = CmsRoute
-- * Creates the value resourcesCms which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Cms. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the CmsRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Cms" $(parseRoutesFile "config/routes")

type CmsRoute = Route Cms

defaultLayoutExtraParents :: [(Maybe (CmsRoute, [(T.Text, T.Text)]), T.Text)] -> GWidget sub Cms () -> GHandler sub Cms RepHtml
defaultLayoutExtraParents parents' widget = do
    mmsg <- getMessage
    mu <- maybeAuth
    (title', parents) <- breadcrumbs
    let fixedParents = map (\(x, y) -> (Just (x, []), y)) parents
    y <- getYesod
    tm <- getRouteToMaster
    cr <- getCurrentRoute
    let isHome = Just RootR == fmap tm cr
    mannouncement <-
        if isHome
            then do
                mb <- runDB $ selectFirst [] [Desc BlogPosted]
                case mb of
                    Nothing -> return Nothing
                    Just (Entity _ b) -> return $ Just [whamlet|
Last blog post: #
<a href=@{BlogR}>#{blogTitle b}
\ #
<span .date>#{formatTime defaultTimeLocale "%B %e, %Y" $ blogPosted b}
|]
            else return Nothing
    pc <- widgetToPageContent $ do
        if isHome
            then setTitle "Yesod Web Framework"
            else setTitle $ toHtml title'
        widget
        addScriptEither $ urlJqueryJs y
        $(widgetFile "comments")
        atomLink BlogFeedR "Blog posts"
        atomLink ContentFeedR "Site activity"
        $(widgetFile "collapse")
    let asString s = s :: String
    pc' <- widgetToPageContent $(widgetFile "default-layout")
    hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Cms where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout = defaultLayoutExtraParents []

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    maximumContentLength _ (Just UserFileR{}) = 7 * 1024 * 1024 -- 7 megabytes
    maximumContentLength _ _ = 2 * 1024 * 1024 -- 2 megabytes

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    -- FIXME gzipCompressFiles _ = True

-- How to run database actions.
instance YesodPersist Cms where
    type YesodPersistBackend Cms = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth Cms where
    type AuthId Cms = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueEmail $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                handle <- getNextHandle 1
                fmap Just $ insert $ User (credsIdent creds) Nothing handle False
      where
        getNextHandle i = do
            let h = "user-" `T.append` T.pack (show (i :: Int))
            x <- getBy $ UniqueHandle h
            maybe (return h) (const $ getNextHandle $ i + 1) x

    authPlugins _ = [authBrowserId]

    authHttpManager = httpManager

instance RenderMessage Cms FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAloha Cms where
    urlAloha _ = Left $ StaticR aloha_aloha_js
    urlAlohaPlugins _ = map (Left . StaticR)
        [ aloha_plugins_com_gentics_aloha_plugins_Format_plugin_js
        , aloha_plugins_com_gentics_aloha_plugins_Table_plugin_js
        , aloha_plugins_com_gentics_aloha_plugins_List_plugin_js
        , aloha_plugins_com_gentics_aloha_plugins_Link_plugin_js
        , aloha_plugins_com_gentics_aloha_plugins_HighlightEditables_plugin_js
        ]

instance YesodJquery Cms where
    urlJqueryJs _ = Left $ StaticR jquery_js

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

type Form x = Html -> MForm Cms Cms (FormResult x, Widget)

fileTitle :: FileStorePath -> GHandler sub Cms T.Text
fileTitle t = do
    Cms { formatHandlers = fhs, fileStore = fs } <- getYesod
    fileTitle' fs fhs t

fileTitle' :: MonadIO m => FileStore -> [FormatHandler master] -> FileStorePath -> m T.Text
fileTitle' fs fhs t = do
    let ext = snd $ T.breakOnEnd "." t
    let mfh = findHandler ext fhs
    muri <- liftIO $ fsGetFile fs t
    mtitle <-
        case (mfh, muri) of
            (Just fh, Just uri) -> liftIO $ fhTitle fh (fsSM fs) uri
            _ -> return Nothing
    return $ fromMaybe backup mtitle
  where
    backup = safeInit $ fst $ T.breakOnEnd "." $ snd $ T.breakOnEnd "/" t
    safeInit s
        | T.null s = s
        | otherwise = T.init s

instance YesodBreadcrumbs Cms where
    breadcrumb RootR = return ("Homepage", Nothing)

    breadcrumb (WikiR []) = return ("Wiki", Just RootR)
    breadcrumb (WikiR x) = do
        let parent = init x
            this = last x
        return (this, Just $ WikiR parent)

    breadcrumb (PageR x []) = return (x, Just RootR)
    breadcrumb (PageR x ys) = return (last ys, Just $ PageR x $ init ys)

    breadcrumb (EditPageR page) = return ("Edit page: " `T.append` (T.intercalate "/" page), Just RootR)
    breadcrumb (DeletePageR page) = return ("Delete page", Just $ EditPageR page)
    breadcrumb ProfileR = return ("Profile", Just RootR)

    breadcrumb UsersR = return ("User list", Just RootR)
    breadcrumb (UserFileR user []) = return (user, Just RootR)
    breadcrumb (UserFileR user x) = do
        -- Check for aliases
        ma <- runDB $ selectFirst [AliasOrig ==. T.intercalate "/" ("home" : user : x)] []
        case ma of
            Nothing -> return (last x, Just $ UserFileR user $ init x)
            Just (Entity _ a) -> return (aliasTitle a, Just RootR)

    breadcrumb BlogArchiveR = return ("Blog", Just RootR)
    breadcrumb (BlogPostR year month slug) = do
        Entity _ b <- runDB $ getBy404 $ UniqueBlog year month slug
        return (blogTitle b, Just BlogArchiveR)

    breadcrumb SearchR = return ("Search", Just RootR)

    breadcrumb StaticR{} = return ("", Nothing)
    breadcrumb AuthR{} = return ("", Nothing)
    breadcrumb FaviconR{} = return ("", Nothing)
    breadcrumb RobotsR{} = return ("", Nothing)
    breadcrumb UserFileIntR{} = return ("", Nothing)
    breadcrumb RedirectorR{} = return ("", Nothing)
    breadcrumb CommentsR = return ("", Nothing)
    breadcrumb CommentCountR = return ("", Nothing)
    breadcrumb BlogFeedR = return ("", Nothing)
    breadcrumb ContentFeedR = return ("", Nothing)
    breadcrumb ContentFeedItemR{} = return ("", Nothing)
    breadcrumb BlogPostNoDateR{} = return ("", Nothing)
    breadcrumb CreateBlogR{} = return ("", Nothing)
    breadcrumb BlogR = return ("", Nothing)
    breadcrumb CreateAliasR{} = return ("", Nothing)
    breadcrumb GroupsR{} = return ("", Nothing)
    breadcrumb DeleteGroupR{} = return ("", Nothing)
    breadcrumb UpGroupR{} = return ("", Nothing)
    breadcrumb DownGroupR{} = return ("", Nothing)
    breadcrumb LabelsR{} = return ("", Nothing)
    breadcrumb DeleteLabelR{} = return ("", Nothing)
    breadcrumb FileLabelsR{} = return ("", Nothing)
    breadcrumb RawR{} = return ("", Nothing)
