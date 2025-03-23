{-# LANGUAGE TemplateHaskell #-}

module Config.Resource
    ( defaultConfigFile
    , defaultServerPort
    ) where

import           Data.ByteString (ByteString)
import           Data.FileEmbed  (embedFile)

defaultConfigFile :: ByteString
defaultConfigFile = $(embedFile "config.yaml")

defaultServerPort :: Int
defaultServerPort = 8080
