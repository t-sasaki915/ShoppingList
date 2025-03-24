module Data.Text.TRead (TRead (..)) where

import           Data.Maybe (fromJust)
import           Data.Text  (Text, unpack)
import           Text.Read  (readMaybe)

class TRead a where
    tReadMaybe :: Text -> Maybe a

    tRead :: Text -> a
    tRead = fromJust . tReadMaybe

instance TRead Int where
    tReadMaybe = readMaybe . unpack

instance TRead Bool where
    tReadMaybe = readMaybe . unpack
