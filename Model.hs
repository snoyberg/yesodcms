{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, GADTs #-}
module Model where

import Yesod
import Data.Text (Text)
import FileStore (FileStorePath)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")
