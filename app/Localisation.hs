module Localisation
    ( Language (..)
    , htmlLanguageCode
    , appTitle
    , editButtonLabel
    , finishedLabel
    , nameLabel
    , amountLabel
    , importanceLabel
    , notesLabel
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

editButtonLabel :: Language -> Text
editButtonLabel English  = "Edit"
editButtonLabel Japanese = "編集"

finishedLabel :: Language -> Text
finishedLabel English  = "Finished"
finishedLabel Japanese = "完了"

nameLabel :: Language -> Text
nameLabel English  = "Name"
nameLabel Japanese = "名称"

amountLabel :: Language -> Text
amountLabel English  = "Amount"
amountLabel Japanese = "個数"

importanceLabel :: Language -> Text
importanceLabel English  = "Importance"
importanceLabel Japanese = "重要性"

notesLabel :: Language -> Text
notesLabel English  = "Notes"
notesLabel Japanese = "備考"
