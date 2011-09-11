{-# LANGUAGE Rank2Types #-}
module FileStore where

import Data.ByteString (ByteString)
import Data.Enumerator (Enumerator, ($$), run_)
import Data.Enumerator.Binary (enumFile, iterHandle)
import qualified Data.Text as T
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS
import Filesystem

type FileStorePath = T.Text

data FileStore = FileStore
    { fsGetFile :: forall a. FileStorePath -> IO (Maybe (Enumerator ByteString IO a))
    , fsPutFile :: FileStorePath -> Enumerator ByteString IO () -> IO ()
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
    }
