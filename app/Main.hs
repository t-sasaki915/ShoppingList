module Main (main) where

import           Data.String.Here     (i)
import           Data.Text.IO         (putStrLn)
import           Data.Version         (showVersion)
import           Paths_ShoppingList   (version)
import           Prelude              hiding (putStrLn)
import           Yesod                hiding (loadConfig)

import           AppConfig            (AppConfig (..))
import           AppConfig.Loader     (loadConfig)
import           Database.Initialiser (initialiseDatabase)
import           WebApp               (initialiseWebApp)

main :: IO ()
main = do
    putStrLn [i|Starting ShoppingList version ${showVersion version} ...|]

    appConfig <- loadConfig "config.yaml"
    database  <- initialiseDatabase "database.db"

    warp (serverPort appConfig) $
        initialiseWebApp appConfig database
