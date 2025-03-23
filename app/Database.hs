module Database
    ( ItemPriority(..)
    , ItemField (..)
    , getAllItems
    , updateItemName
    , updateItemAmount
    , updateItemPriority
    , updateItemNotes
    , updateItemIsFinished
    ) where

import           Data.Text                        (Text, pack)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField (FromField (..))
import           Database.SQLite.Simple.Internal  (Field (..))
import           Database.SQLite.Simple.ToField   (ToField (..))
import           Localisation                     (Language (..),
                                                   Localisable (..))
import           Text.Printf                      (printf)

data ItemPriority = High | Normal | Low deriving Show

instance Localisable ItemPriority where
    localise High English    = "High"
    localise Normal English  = "Normal"
    localise Low English     = "Low"

    localise High Japanese   = "高"
    localise Normal Japanese = "普通"
    localise Low Japanese    = "低"

instance FromField ItemPriority where
    fromField (Field (SQLText "High") _)   = pure High
    fromField (Field (SQLText "Normal") _) = pure Normal
    fromField (Field (SQLText "Low") _)    = pure Low
    fromField (Field x _)                  = fail (printf "Unrecognisable priority: '%s'" (show x))

instance ToField ItemPriority where
    toField = SQLText . pack . show

data ItemField = ItemField
    { itemId         :: Int
    , itemName       :: Text
    , itemAmount     :: Int
    , itemPriority   :: ItemPriority
    , itemNotes      :: Maybe Text
    , itemIsFinished :: Bool
    }
    deriving Show

instance FromRow ItemField where
    fromRow = ItemField <$> field <*> field <*> field <*> field <*> field <*> field

getAllItems :: Connection -> IO [ItemField]
getAllItems = flip query_ "SELECT * FROM shopping_list"

updateItemName :: Connection -> Int -> Text -> IO ()
updateItemName database iid newName =
    execute database "UPDATE shopping_list SET name = ? WHERE id = ?" (newName, iid)

updateItemAmount :: Connection -> Int -> Int -> IO ()
updateItemAmount database iid newAmount =
    execute database "UPDATE shopping_list SET amount = ? WHERE id = ?" (newAmount, iid)

updateItemPriority :: Connection -> Int -> ItemPriority -> IO ()
updateItemPriority database iid newPriority =
    execute database "UPDATE shopping_list SET priority = ? WHERE id = ?" (newPriority, iid)

updateItemNotes :: Connection -> Int -> Text -> IO ()
updateItemNotes database iid newNotes =
    execute database "UPDATE shopping_list SET notes = ? WHERE id = ?" (newNotes, iid)

updateItemIsFinished :: Connection -> Int -> Bool -> IO ()
updateItemIsFinished database iid newIsFinished =
    execute database "UPDATE shopping_list SET is_finished = ? WHERE id = ?" (newIsFinished, iid)
