module Database.WebApp(handleDB) where

import           Data.Functor           ((<&>))
import           Database.SQLite.Simple (Connection)
import           Yesod                  (HandlerFor, getYesod, liftIO)

import           WebApp                 (WebApp (databaseConnection))

handleDB :: (Connection -> IO a) -> (HandlerFor WebApp) a
handleDB f = getYesod <&> databaseConnection >>= liftIO . f
