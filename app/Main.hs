{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import           Data.Functor           ((<&>))
import           Data.Version           (showVersion)
import           Database.SQLite.Simple (Connection)
import           Paths_ShoppingList     (version)
import           Text.Printf            (printf)
import           Yesod                  hiding (loadConfig)

import           AppConfig              (AppConfig (..))
import           AppConfig.Loader       (loadConfig)
import           Database.Initialiser   (initialiseDatabase)
import           Localisation

data ShoppingList = ShoppingList
    { interfaceLanguage  :: Language
    , databaseConnection :: Connection
    }

mkYesod "ShoppingList" [parseRoutes|
/ HomeR GET
|]

instance Yesod ShoppingList

getHomeR :: Handler Html
getHomeR = do
    interfaceLang <- getYesod <&> interfaceLanguage

    defaultLayout $ do
        setTitle (localiseHtml AppTitle interfaceLang)
        setLanguage (htmlLanguageCode interfaceLang)

        toWidgetHead
            [hamlet|
                <meta charset="utf-8">
                <meta name="viewport", content="width=device-width,initial-scale=1">
                <link rel="stylesheet", href="style.css">
            |]

main :: IO ()
main = do
    putStrLn $ printf "Starting ShoppingList version %s ..." (showVersion version)

    appConfig <- loadConfig "config.yaml"
    database  <- initialiseDatabase "database.db"

    warp (serverPort appConfig) $
        ShoppingList
            { interfaceLanguage  = webInterfaceLanguage appConfig
            , databaseConnection = database
            }
