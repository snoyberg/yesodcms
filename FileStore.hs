{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module FileStore where

import Data.ByteString (ByteString)
import Data.Conduit (Source (..), ($$), runResourceT, ResourceIO)
import Data.Conduit.Binary (sourceFile, sinkFile)
import qualified Data.Text as T
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS
import Filesystem
import Control.Monad (forM, when)
import Network.URI.Conduit
import qualified Data.Set as Set
import Control.Monad.IO.Class (liftIO)

type FileStorePath = T.Text

data FileStore = FileStore
    { fsGetFile :: FileStorePath -> IO (Maybe URI)
    , fsPutFile :: FileStorePath -> Source IO ByteString -> IO ()
    , fsDelete :: FileStorePath -> IO ()
    , fsList :: FileStorePath -> IO [(T.Text, Bool)] -- ^ is it a folder?
    , fsMkdir :: FileStorePath -> IO ()
    , fsSM :: SchemeMap
    , fsFromURI :: URI -> Maybe FileStorePath
    }

simpleFileStore :: FilePath -> FileStore
simpleFileStore dir = FileStore
    { fsGetFile = \t -> do
        let fp = dir </> fromText t
        e <- isFile fp
        if e
            then return $ Just nullURI
                { uriScheme = "fs:"
                , uriPath = t
                }
            else return Nothing
    , fsPutFile = \t enum -> do
        let fp = dir </> fromText t
        createTree $ directory fp
        runResourceT $ enum $$ sinkFile (encodeString fp)
    , fsDelete = \t -> do
        let fp = dir </> fromText t
        f <- isFile fp
        when f $ removeFile fp
    , fsList = \t -> do
        let dir' = dir </> fromText t
        d <- isDirectory dir'
        fps <- if d then listDirectory dir' else return []
        forM fps $ \fp -> do
            f <- isFile fp
            return (either id id $ toText $ filename fp, not f)
    , fsMkdir = createTree . (dir </>) . fromText
    , fsSM = toSchemeMap $ return $ Scheme
        { schemeNames = Set.singleton "fs:"
        , schemeReader = Just $ \uri ->
            let t = uriPath uri
             in tryFiles dir uri [t, T.replace "%20" " " t]
        , schemeWriter = Nothing
        }
    , fsFromURI = \u -> if uriScheme u == "fs:" then Just (uriPath u) else Nothing
    }

tryFiles :: ResourceIO m => FilePath -> URI -> [T.Text] -> Source m ByteString
tryFiles _ uri [] = Source
    { sourcePull = error $ "File not found: " ++ show (toNetworkURI uri)
    , sourceClose = return ()
    }
tryFiles dir uri (t:ts) = Source
    { sourcePull = do
        let fp = dir </> fromText t
        e <- liftIO $ isFile fp
        sourcePull $ if e
            then sourceFile (encodeString fp)
            else tryFiles dir uri ts
    , sourceClose = return ()
    }
