module Database
    ( ItemPriority(..)
    , ItemField (..)
    , getAllItems
    ) where

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField (FromField (..))
import           Database.SQLite.Simple.Internal  (Field (..))
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

data ItemField = ItemField
    { itemId         :: Int
    , itemName       :: String
    , itemAmount     :: Int
    , itemPriority   :: ItemPriority
    , itemNotes      :: Maybe String
    , itemIsFinished :: Bool
    }
    deriving Show

instance FromField ItemPriority where
    fromField (Field (SQLText "HIGH") _)   = pure High
    fromField (Field (SQLText "NORMAL") _) = pure Normal
    fromField (Field (SQLText "LOW") _)    = pure Low
    fromField (Field x _)                  = fail (printf "Unrecognisable priority: '%s'" (show x))

instance FromRow ItemField where
    fromRow = ItemField <$> field <*> field <*> field <*> field <*> field <*> field

getAllItems :: Connection -> IO [ItemField]
getAllItems = flip query_ "SELECT * FROM shopping_list"
