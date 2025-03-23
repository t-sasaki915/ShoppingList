{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Localisation where

import           Data.Yaml   (FromJSON (..), Value (..))
import           Text.Printf (printf)

data Language = English
              | Japanese
              deriving Show

instance FromJSON Language where
    parseJSON (String "English")  = pure English
    parseJSON (String "Japanese") = pure Japanese
    parseJSON x                   = fail (printf "Unrecognisable language '%s'." (show x))

appTitle :: Language -> String
appTitle English  = "ShoppingList"
appTitle Japanese = "買い物リスト"
