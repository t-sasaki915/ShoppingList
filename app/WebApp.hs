{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module WebApp
    ( WebApp (..)
    , Route (..)
    , defaultWebAppLayout
    , initialiseWebApp
    ) where

import                          Database.SQLite.Simple    (Connection)
import                          Yesod

import                          AppConfig                 (AppConfig (..))
import                          Localisation              (Language)
import {-# SOURCE #-}           WebApp.HomeR              (getHomeR)
import {-# SOURCE #-}           WebApp.OrderOptionUpdateR (getOrderOptionUpdateR)
import                          WebApp.StyleSheet         (commonStyleSheet)

data WebApp = WebApp
    { interfaceLanguage  :: Language
    , databaseConnection :: Connection
    }

mkYesod "WebApp" [parseRoutes|
/                    HomeR GET
/setting/orderOption OrderOptionUpdateR GET
|]

instance Yesod WebApp

instance RenderMessage WebApp FormMessage where
    renderMessage _ _ = defaultFormMessage

defaultWebAppLayout :: WidgetFor WebApp () -> Handler Html
defaultWebAppLayout content =
    defaultLayout $ do
        setTitle "ShoppingList"

        toWidgetHead
            [hamlet|
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width,initial-scale=1">
            |]

        toWidgetHead
            [julius|
                window.addEventListener("load", () => {
                    Array
                        .from(document.getElementsByClassName("submitOnChange"))
                        .forEach(elem => {
                            elem.addEventListener("change", () => elem.form.submit());
                        });
                });
            |]

        toWidgetHead commonStyleSheet

        content

initialiseWebApp :: AppConfig -> Connection -> WebApp
initialiseWebApp appConfig dbConnection =
    WebApp
        { interfaceLanguage  = webInterfaceLanguage appConfig
        , databaseConnection = dbConnection
        }
