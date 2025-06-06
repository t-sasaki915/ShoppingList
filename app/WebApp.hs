{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module WebApp
    ( WebApp (..)
    , defaultWebAppLayout
    , initialiseWebApp
    ) where

import                          Database.SQLite.Simple (Connection)
import                          Yesod

import                          AppConfig              (AppConfig (..))
import                          Localisation           (Language)
import {-# SOURCE #-}           WebApp.HomeR           (getHomeR)
import                          WebApp.StyleSheet      (commonStyleSheet)

data WebApp = WebApp
    { interfaceLanguage  :: Language
    , databaseConnection :: Connection
    }

mkYesod "WebApp" [parseRoutes|
/ HomeR GET
|]

instance Yesod WebApp

defaultWebAppLayout :: WidgetFor WebApp () -> HandlerFor WebApp Html
defaultWebAppLayout content =
    defaultLayout $ do
        setTitle "ShoppingList"

        toWidgetHead
            [hamlet|
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width,initial-scale=1">
            |]

        toWidgetHead commonStyleSheet

        content

initialiseWebApp :: AppConfig -> Connection -> WebApp
initialiseWebApp appConfig dbConnection =
    WebApp
        { interfaceLanguage  = webInterfaceLanguage appConfig
        , databaseConnection = dbConnection
        }
