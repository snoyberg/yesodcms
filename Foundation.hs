{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
module Foundation
    ( Cms (..)
    , CmsRoute (..)
    , resourcesCms
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Yesod
    , module Settings
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
    ) where

import Yesod
import Yesod.Static (Static, base64md5, StaticRoute(..))
import Settings.StaticFiles
import Yesod.Auth
import Yesod.Auth.BrowserId (authBrowserId')
import Yesod.Logger (Logger, logLazyText)
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Database.Persist.GenericSql
import Settings (hamletFile, cassiusFile, luciusFile, juliusFile, widgetFile)
import Model
import Control.Monad (unless)
import qualified Data.Text.Lazy.Encoding
import Text.Jasmine (minifym)
import qualified Data.Text as T
import Web.ClientSession (getKey)
import Data.Text (Text)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Cms = Cms
    { settings :: Settings.AppConfig
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Settings.ConnectionPool -- ^ Database connection pool.
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

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Cms where
    approot = Settings.appRoot . settings

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        mmsg <- getMessage
        mu <- maybeAuth
        (title', parents) <- breadcrumbs
        pc <- widgetToPageContent $ do
            $(widgetFile "top-bar")
            widget
        hamletToRepHtml $(hamletFile "default-layout")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogMessage loc level msg >>= logLazyText (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : T.unpack ext'
        let content' =
                if ext' == "js"
                    then case minifym content of
                            Left _ -> content
                            Right y -> y
                    else content
        let statictmp = Settings.staticDir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", T.pack fn] [], [])


-- How to run database actions.
instance YesodPersist Cms where
    type YesodPersistBackend Cms = SqlPersist
    runDB f = liftIOHandler
            $ fmap connPool getYesod >>= Settings.runConnectionPool f

instance YesodAuth Cms where
    type AuthId Cms = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (uid, _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User $ credsIdent creds

    authPlugins = [authBrowserId']

-- Sends off your mail. Requires sendmail in production!
deliver :: Cms -> L.ByteString -> IO ()
#ifdef PRODUCTION
deliver _ = sendmail
#else
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#endif

instance RenderMessage Cms FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodBreadcrumbs Cms where
    breadcrumb RootR = return ("Homepage", Nothing)

    breadcrumb StaticR{} = return ("", Nothing)
    breadcrumb AuthR{} = return ("", Nothing)
    breadcrumb FaviconR{} = return ("", Nothing)
    breadcrumb RobotsR{} = return ("", Nothing)
