{-# LANGUAGE TemplateHaskell #-}

module WebServer.Resource (styleSheetFile) where

import           Data.ByteString (ByteString)
import           Data.FileEmbed  (embedFile)

styleSheetFile :: ByteString
styleSheetFile = $(embedFile "style.css")
