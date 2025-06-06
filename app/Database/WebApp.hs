module Database.WebApp
    ( handleDB
    , handleDB1
    , handleDB2
    , handleDB3
    , handleDB4
    ) where

import           Database.SQLite.Simple (Connection)
import           Yesod                  (HandlerFor, getYesod, liftIO)

import           WebApp                 (WebApp (databaseConnection))

handleDB :: (Connection -> IO a) -> (HandlerFor WebApp) a
handleDB f = getYesod >>= (liftIO . f) . databaseConnection

handleDB1 :: (a -> Connection -> IO b) -> a -> (HandlerFor WebApp) b
handleDB1 f a = getYesod >>= (liftIO . f a) . databaseConnection

handleDB2 :: (a -> b -> Connection -> IO c) -> a -> b -> (HandlerFor WebApp) c
handleDB2 f a b = getYesod >>= (liftIO . f a b) . databaseConnection

handleDB3 :: (a -> b -> c -> Connection -> IO d) -> a -> b -> c -> (HandlerFor WebApp) d
handleDB3 f a b c = getYesod >>= (liftIO . f a b c) . databaseConnection

handleDB4 :: (a -> b -> c -> d -> Connection -> IO e) -> a -> b -> c -> d -> (HandlerFor WebApp) e
handleDB4 f a b c d = getYesod >>= (liftIO . f a b c d) . databaseConnection
