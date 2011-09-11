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
import Control.Monad (forM)
import Network.URI.Enumerator
import qualified Data.Set as Set

type FileStorePath = T.Text

data FileStore = FileStore
    { fsGetFile :: FileStorePath -> IO (Maybe URI)
    , fsPutFile :: FileStorePath -> Enumerator ByteString IO () -> IO ()
    , fsList :: FileStorePath -> IO [(T.Text, Bool)] -- ^ is it a folder?
    , fsMkdir :: FileStorePath -> IO ()
    , fsSM :: SchemeMap IO
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
        , schemeReader = Just $ enumFile . encodeString . (dir </>) . fromText . uriPath
        , schemeWriter = Nothing
        }
    }
