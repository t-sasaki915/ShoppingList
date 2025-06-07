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
    , updateShouldHideDoneItems
    , updateItemOrder
    ) where

import           Data.Functor           ((<&>))
import           Data.Text              (Text)
import           Database.SQLite.Simple (Connection, Only (..), execute, query,
                                         query_)

import           Item                   (ItemField, ItemOrder, ItemOrderOption,
                                         ItemPriority)

getAllItems :: Connection -> IO [ItemField]
getAllItems = flip query_ "SELECT * FROM shopping_list"

updateItemName :: Int -> Text -> Connection -> IO ()
updateItemName iid newName database=
    execute database "UPDATE shopping_list SET name = ? WHERE id = ?" (newName, iid)

updateItemAmount :: Int -> Int -> Connection -> IO ()
updateItemAmount iid newAmount database =
    execute database "UPDATE shopping_list SET amount = ? WHERE id = ?" (newAmount, iid)

updateItemPriority :: Int -> ItemPriority -> Connection -> IO ()
updateItemPriority iid newPriority database =
    execute database "UPDATE shopping_list SET priority = ? WHERE id = ?" (newPriority, iid)

updateItemNotes :: Int -> Text -> Connection -> IO ()
updateItemNotes iid newNotes database =
    execute database "UPDATE shopping_list SET notes = ? WHERE id = ?" (newNotes, iid)

updateItemIsFinished :: Int -> Bool -> Connection -> IO ()
updateItemIsFinished iid newIsFinished database =
    execute database "UPDATE shopping_list SET is_finished = ? WHERE id = ?" (newIsFinished, iid)

deleteItem :: Int -> Connection -> IO ()
deleteItem iid database =
    execute database "DELETE FROM shopping_list WHERE id = ?" (Only iid)

addItem :: Text -> Int -> ItemPriority -> Maybe Text -> Connection -> IO ()
addItem newName newAmount newPriority newNotes database =
    execute database
        "INSERT INTO shopping_list (name, amount, priority, notes, is_finished) VALUES (?, ?, ?, ?, 0)"
        (newName, newAmount, newPriority, newNotes)

getItem :: Int -> Connection -> IO ItemField
getItem iid database =
    query database "SELECT * FROM shopping_list WHERE id = ?" (Only iid) <&> head

getItemOrderOption :: Connection -> IO ItemOrderOption
getItemOrderOption database =
    query_ database "SELECT hide_done_items, item_order FROM user_setting" <&> head

updateShouldHideDoneItems :: Bool -> Connection -> IO ()
updateShouldHideDoneItems newBool database =
    execute database "UPDATE user_setting SET hide_done_items = ?" (Only newBool)

updateItemOrder :: ItemOrder -> Connection -> IO ()
updateItemOrder newItemOrder database =
    execute database "UPDATE user_setting SET item_order = ?" (Only newItemOrder)
