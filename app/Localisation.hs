module Localisation
    ( Language (..)
    , htmlLanguageCode
    , appTitle
    , versionLabel
    , editButtonLabel
    ) where

import           Data.Text   (Text)
import           Data.Yaml   (FromJSON (..), Value (..))
import           Text.Printf (printf)

data Language = English
              | Japanese
              deriving Show

instance FromJSON Language where
    parseJSON (String "English")  = pure English
    parseJSON (String "Japanese") = pure Japanese
    parseJSON x                   = fail (printf "Unrecognisable language '%s'." (show x))

htmlLanguageCode :: Language -> Text
htmlLanguageCode English  = "en"
htmlLanguageCode Japanese = "ja"

appTitle :: Language -> Text
appTitle English  = "ShoppingList"
appTitle Japanese = "買い物リスト"

versionLabel :: Language -> Text
versionLabel English  = " version "
versionLabel Japanese = " バージョン "

editButtonLabel :: Language -> Text
editButtonLabel English  = "Edit"
editButtonLabel Japanese = "編集"
