{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Model where

import Yesod
import Data.Text (Text, pack)
import FileStore (FileStorePath)
import Data.Time (UTCTime)
import Text.Hamlet (shamlet)
import qualified Yesod.Goodies.Gravatar as G
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Network.URI.Enumerator

newtype BlogSlugT = BlogSlugT Text
    deriving (Read, Eq, Show, PersistField, SinglePiece, Ord)
newtype Month = Month Int
    deriving (Read, Eq, Show, PersistField, Ord)
instance SinglePiece Month where
    toSinglePiece (Month i)
        | i < 10 && i >= 0 = pack $ '0' : show i
        | otherwise = toSinglePiece i
    fromSinglePiece t = do
        i <- fromSinglePiece t
        if i >= 1 && i <= 12
            then Just $ Month i
            else Nothing

share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")

userDisplayName :: User -> Html
userDisplayName u = [shamlet|
$maybe n <- userName u
    \#{n} #
    <i>#{userHandle u}
$nothing
    \#{userHandle u}
|]

userGravatar :: Int -> User -> String
userGravatar size u = G.gravatarImg (userEmail u) G.defaultOptions
    { G.gSize = Just $ G.Size size
    , G.gDefault = Just G.MM
    }

getFileNameId :: (PersistBackend b m, Functor (b m)) => Text -> b m (Key b (FileNameGeneric backend))
getFileNameId t =
    fmap (either fst id) $ insertBy $ FileName (T.append "fs:" $ fromMaybe t $ T.stripPrefix "fs:" t) Nothing Nothing

getFileNameIdURI :: (PersistBackend b m, Functor (b m)) => URI -> b m (Key b (FileNameGeneric backend))
getFileNameIdURI = getFileNameId . T.replace "%20" " " . uriPath

addLabel :: (PersistBackend b m, Functor (b m)) => Key b FileName -> T.Text -> b m ()
addLabel fid name = do
    lids <- fmap (map fst) $ selectList [LabelName ==. name] []
    mapM_ (insert . FileLabel fid) lids
