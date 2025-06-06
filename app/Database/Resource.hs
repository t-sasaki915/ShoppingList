{-# LANGUAGE TemplateHaskell #-}

module Database.Resource (defaultDatabaseFile) where

import           Data.ByteString (ByteString)
import           Data.FileEmbed  (embedFile)

defaultDatabaseFile :: ByteString
defaultDatabaseFile = $(embedFile "defaultDB.db")
