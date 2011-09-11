{-# LANGUAGE CPP, DeriveDataTypeable #-}
import qualified Settings as Settings
import Settings (AppConfig(..))
import Application (withCms)
import Network.Wai.Handler.Warp (run)
import System.Console.CmdArgs hiding (args)
import Data.Char (toUpper, toLower)
import Yesod.Logger (logString, logLazyText, flushLogger, makeLogger)

#ifndef PRODUCTION
import Network.Wai.Middleware.Debug (debugHandle)
#endif

main :: IO ()
main = do
    logger <- makeLogger
    args <- cmdArgs argConfig
    env <- getAppEnv args
    config <- Settings.loadConfig env
    let c = if (port args) /= 0 then config {appPort = (port args) } else config
#if PRODUCTION
    withCms c $ run (appPort c)
#else
    logString logger $ (show env) ++ " application launched, listening on port " ++ show (appPort c)
    withCms c logger $ run (appPort c) . debugHandle (logHandle logger)
    flushLogger logger
#endif
  where
    logHandle logger msg = logLazyText logger msg >> flushLogger logger

data ArgConfig = ArgConfig {environment :: String, port :: Int}
                 deriving (Show, Data, Typeable)

argConfig :: ArgConfig
argConfig = ArgConfig{ environment = def 
  &= help ("application environment, one of: " ++ (foldl1 (\a b -> a ++ ", " ++ b) environments))
  &= typ "ENVIRONMENT"
  ,port = def &= typ "PORT"
}

environments :: [String]
environments = map ((map toLower) . show) ([minBound..maxBound] :: [Settings.AppEnvironment])

-- | retrieve the -e environment option
getAppEnv :: ArgConfig ->  IO Settings.AppEnvironment
getAppEnv cfg = do
    let e = if (environment cfg) /= "" then (environment cfg)
            else
#if PRODUCTION
                  "production"
#else
                  "development"
#endif
    return $ read $ capitalize e
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : map toLower xs