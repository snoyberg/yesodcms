{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Model where

import Yesod
import Data.Text (Text, pack)
import FileStore (FileStorePath)
import Data.Time (UTCTime)
import Text.Hamlet (shamlet)

newtype BlogSlugT = BlogSlugT Text
    deriving (Read, Eq, Show, PersistField, PathPiece, Ord)
newtype Month = Month Int
    deriving (Read, Eq, Show, PersistField, Ord)
instance PathPiece Month where
    toPathPiece (Month i)
        | i < 10 && i >= 0 = pack $ '0' : show i
        | otherwise = toPathPiece i
    fromPathPiece t = do
        i <- fromPathPiece t
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
