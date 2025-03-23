module Main (main) where

import           Config.Loader       (loadConfig)
import           Data.Version        (showVersion)
import           Paths_ShoppingList  (version)
import           Text.Printf         (printf)
import           WebServer.WebServer (startWebServer)

main :: IO ()
main = do
    putStrLn $ printf "Starting ShoppingList version %s ..." (showVersion version)

    appConfig <- loadConfig "config.yaml"

    startWebServer appConfig
