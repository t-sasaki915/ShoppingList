module Localisation
    ( Language (..)
    , Localisable (..)
    , htmlLanguageCode
    , appTitle
    , editButtonLabel
    , deleteButtonLabel
    , viewButtonLabel
    , finishedLabel
    , nameLabel
    , amountLabel
    , priorityLabel
    , notesLabel
    , operationLabel
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

class Localisable a where
    localise :: a -> Language -> Text

htmlLanguageCode :: Language -> Text
htmlLanguageCode English  = "en"
htmlLanguageCode Japanese = "ja"

appTitle :: Language -> Text
appTitle English  = "ShoppingList"
appTitle Japanese = "買い物リスト"

editButtonLabel :: Language -> Text
editButtonLabel English  = "Edit"
editButtonLabel Japanese = "編集"

deleteButtonLabel :: Language -> Text
deleteButtonLabel English  = "Delete"
deleteButtonLabel Japanese = "削除"

viewButtonLabel :: Language -> Text
viewButtonLabel English  = "View"
viewButtonLabel Japanese = "閲覧"

finishedLabel :: Language -> Text
finishedLabel English  = "Finished"
finishedLabel Japanese = "完了"

nameLabel :: Language -> Text
nameLabel English  = "Name"
nameLabel Japanese = "名称"

amountLabel :: Language -> Text
amountLabel English  = "Amount"
amountLabel Japanese = "個数"

priorityLabel :: Language -> Text
priorityLabel English  = "Importance"
priorityLabel Japanese = "重要性"

notesLabel :: Language -> Text
notesLabel English  = "Notes"
notesLabel Japanese = "備考"

operationLabel :: Language -> Text
operationLabel English  = "Operation"
operationLabel Japanese = "操作"
