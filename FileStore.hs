{-# LANGUAGE Rank2Types #-}
module FileStore where

import Data.ByteString (ByteString)
import Data.Enumerator (Enumerator, ($$), run_)
import Data.Enumerator.Binary (enumFile, iterHandle)
import qualified Data.Text as T
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS
import Filesystem
import Control.Monad (forM)

type FileStorePath = T.Text

data FileStore = FileStore
    { fsGetFile :: forall a. FileStorePath -> IO (Maybe (Enumerator ByteString IO a))
    , fsPutFile :: FileStorePath -> Enumerator ByteString IO () -> IO ()
    , fsList :: FileStorePath -> IO [(T.Text, Bool)] -- ^ is it a folder?
    , fsMkdir :: FileStorePath -> IO ()
    }

simpleFileStore :: FilePath -> FileStore
simpleFileStore dir = FileStore
    { fsGetFile = \t -> do
        let fp = dir </> fromText t
        e <- isFile fp
        if e
            then return $ Just $ enumFile (encodeString fp)
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
    }
