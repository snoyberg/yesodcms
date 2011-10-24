{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module FileStore where

import Data.ByteString (ByteString)
import Data.Enumerator (Enumerator, ($$), run_)
import Data.Enumerator.Binary (enumFile, iterHandle)
import qualified Data.Text as T
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS
import Filesystem
import Control.Monad (forM, when)
import Network.URI.Enumerator
import qualified Data.Set as Set
import Control.Monad.IO.Class (liftIO)

type FileStorePath = T.Text

data FileStore = FileStore
    { fsGetFile :: FileStorePath -> IO (Maybe URI)
    , fsPutFile :: FileStorePath -> Enumerator ByteString IO () -> IO ()
    , fsDelete :: FileStorePath -> IO ()
    , fsList :: FileStorePath -> IO [(T.Text, Bool)] -- ^ is it a folder?
    , fsMkdir :: FileStorePath -> IO ()
    , fsSM :: SchemeMap IO
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
        withFile fp WriteMode $ \h -> run_ $ enum $$ iterHandle h
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

tryFiles :: FilePath -> URI -> [T.Text] -> Enumerator ByteString IO a
tryFiles _ uri [] _ = error $ "File not found: " ++ show (toNetworkURI uri)
tryFiles dir uri (t:ts) step = do
    let fp = dir </> fromText t
    e <- liftIO $ isFile fp
    if e
        then enumFile (encodeString fp) step
        else tryFiles dir uri ts step
