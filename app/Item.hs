module Item
    ( ItemPriority (..)
    , ItemField (..)
    , ItemOrder (..)
    , ItemOrderOption (..)
    , compareItemFieldByPriority
    ) where

import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Text.TRead                  (TRead (..))
import           Database.SQLite.Simple           (FromRow (..), SQLData (..),
                                                   field)
import           Database.SQLite.Simple.FromField (FromField (..))
import           Database.SQLite.Simple.Internal  (Field (..))
import           Database.SQLite.Simple.ToField   (ToField (..))
import           Localisation                     (Language (..),
                                                   Localisable (..))
import           Text.Printf                      (printf)

data ItemPriority = High | Normal | Low deriving (Show, Eq)

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
    toField = SQLText . Text.show

instance TRead ItemPriority where
    tReadMaybe "High"   = Just High
    tReadMaybe "Normal" = Just Normal
    tReadMaybe "Low"    = Just Low
    tReadMaybe _        = Nothing

instance Ord ItemPriority where
    compare High High     = EQ
    compare Normal Normal = EQ
    compare Low Low       = EQ

    compare Low Normal    = LT
    compare Normal High   = LT
    compare Low High      = LT

    compare High Normal   = GT
    compare Normal Low    = GT
    compare High Low      = GT

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

compareItemFieldByPriority :: ItemField -> ItemField -> Ordering
compareItemFieldByPriority a b = compare (itemPriority b) (itemPriority a)

data ItemOrder = DefaultOrder
               | PriorityOrder
               deriving (Show, Eq)

instance Localisable ItemOrder where
    localise DefaultOrder English   = "Default"
    localise PriorityOrder English  = "By Priority"

    localise DefaultOrder Japanese  = "デフォルト"
    localise PriorityOrder Japanese = "重要性順"

instance FromField ItemOrder where
    fromField (Field (SQLText "DefaultOrder") _)  = pure DefaultOrder
    fromField (Field (SQLText "PriorityOrder") _) = pure PriorityOrder
    fromField (Field x _) = fail (printf "Unrecognisable order: '%s'" (show x))

instance ToField ItemOrder where
    toField = SQLText . Text.show

instance TRead ItemOrder where
    tReadMaybe "DefaultOrder"  = Just DefaultOrder
    tReadMaybe "PriorityOrder" = Just PriorityOrder
    tReadMaybe _               = Nothing

data ItemOrderOption = ItemOrderOption
    { shouldHideDoneItems :: Bool
    , itemOrder           :: ItemOrder
    }
    deriving Show

instance FromRow ItemOrderOption where
    fromRow = ItemOrderOption <$> field <*> field
