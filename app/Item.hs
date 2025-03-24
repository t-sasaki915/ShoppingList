module Item (ItemPriority (..), ItemField (..)) where

import           Data.Text                        (Text, pack)
import           Database.SQLite.Simple           (FromRow (..), SQLData (..),
                                                   field)
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
