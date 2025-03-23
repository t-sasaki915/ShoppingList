{-# LANGUAGE TemplateHaskell #-}

module AppConfig.Resource
    ( defaultConfigFile
    , defaultServerPort
    , defaultWebInterfaceLanguage
    ) where

import           Data.ByteString (ByteString)
import           Data.FileEmbed  (embedFile)
import           Localisation    (Language (English))

defaultConfigFile :: ByteString
defaultConfigFile = $(embedFile "config.yaml")

defaultServerPort :: Int
defaultServerPort = 8080

defaultWebInterfaceLanguage :: Language
defaultWebInterfaceLanguage = English
