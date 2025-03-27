module Database
    ( getAllItems
    , updateItemName
    , updateItemAmount
    , updateItemPriority
    , updateItemNotes
    , updateItemIsFinished
    , deleteItem
    , addItem
    , getItem
    , getItemOrderOption
    ) where

import           Data.Functor           ((<&>))
import           Data.Text              (Text)
import           Database.SQLite.Simple (Connection, Only (..), execute, query,
                                         query_)
import           Item                   (ItemField, ItemOrderOption,
                                         ItemPriority)

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

deleteItem :: Connection -> Int -> IO ()
deleteItem database iid =
    execute database "DELETE FROM shopping_list WHERE id = ?" (Only iid)

addItem :: Connection -> Text -> Int -> ItemPriority -> Text -> IO ()
addItem database newName newAmount newPriority newNotes =
    execute database
        "INSERT INTO shopping_list (name, amount, priority, notes, is_finished) VALUES (?, ?, ?, ?, 0)"
        (newName, newAmount, newPriority, newNotes)

getItem :: Connection -> Int -> IO ItemField
getItem database iid =
    query database "SELECT * FROM shopping_list WHERE id = ?" (Only iid) <&> head

getItemOrderOption :: Connection -> IO ItemOrderOption
getItemOrderOption database =
    query_ database "SELECT hide_done_items, item_order FROM user_setting WHERE name = 'order_options'" <&> head
