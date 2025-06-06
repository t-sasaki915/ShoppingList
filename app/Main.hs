module Main (main) where

import           Data.Version         (showVersion)
import           Paths_ShoppingList   (version)
import           Text.Printf          (printf)
import           Yesod                hiding (loadConfig)

import           AppConfig            (AppConfig (..))
import           AppConfig.Loader     (loadConfig)
import           Database.Initialiser (initialiseDatabase)
import           WebApp               (initialiseWebApp)

main :: IO ()
main = do
    putStrLn $ printf "Starting ShoppingList version %s ..." (showVersion version)

    appConfig <- loadConfig "config.yaml"
    database  <- initialiseDatabase "database.db"

    warp (serverPort appConfig) $
        initialiseWebApp appConfig database
