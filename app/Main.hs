{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import           Data.Functor           ((<&>))
import           Data.Version           (showVersion)
import           Database.SQLite.Simple (Connection)
import           Lucid
import           Paths_ShoppingList     (version)
import           Text.Printf            (printf)
import           Yesod                  hiding (loadConfig)
import           Yesod.Lucid            (LucidHtml, lucid)

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

getHomeR :: Handler LucidHtml
getHomeR = do
    interfaceLang <- getYesod <&> interfaceLanguage

    lucid $ const $ do
        doctype_
        html_ [lang_ (htmlLanguageCode interfaceLang)] $ do
            head_ [] $ do
                meta_ [charset_ "utf-8"]
                meta_ [name_ "viewport", content_ "width=device-width,initial-scale=1"]
                link_ [rel_ "stylesheet", href_ "style.css"]
                title_ (localiseHtml AppTitle interfaceLang)

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
