module Main (main) where

import           AppConfig.Loader     (loadConfig)
import           Data.Version         (showVersion)
import           Database.Initialiser (initialiseDatabase)
import           Paths_ShoppingList   (version)
import           Text.Printf          (printf)
import           WebServer            (startWebServer)

main :: IO ()
main = do
    putStrLn $ printf "Starting ShoppingList version %s ..." (showVersion version)

    appConfig <- loadConfig "config.yaml"
    database  <- initialiseDatabase "database.db"

    startWebServer appConfig
