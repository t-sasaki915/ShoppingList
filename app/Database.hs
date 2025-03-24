module Database
    ( getAllItems
    , updateItemName
    , updateItemAmount
    , updateItemPriority
    , updateItemNotes
    , updateItemIsFinished
    ) where

import           Data.Text              (Text)
import           Database.SQLite.Simple (Connection, execute, query_)
import           Item                   (ItemField, ItemPriority)

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
