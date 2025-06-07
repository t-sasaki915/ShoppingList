{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module WebApp
    ( WebApp (..)
    , Route (..)
    , defaultWebAppLayout
    , initialiseWebApp
    ) where

import                          Data.Text              (unpack)
import                qualified Data.Text              as Text
import                          Database.SQLite.Simple (Connection)
import                          Network.URI.Encode     (decodeText, encodeText)
import                          Yesod

import                          AppConfig              (AppConfig (..))
import                          Localisation           (Language)
import {-# SOURCE #-}           WebApp.AddR            (getAddR, postAddR)
import {-# SOURCE #-}           WebApp.EditR           (getEditR)
import {-# SOURCE #-}           WebApp.HomeR           (getHomeR)
import {-# SOURCE #-}           WebApp.ManageR         (getManageR)
import {-# SOURCE #-}           WebApp.ModifyR         (ModifyAction,
                                                        postModifyR)
import {-# SOURCE #-}           WebApp.SettingUpdateR  (postSettingUpdateR)
import                          WebApp.StyleSheet      (commonStyleSheet)

data WebApp = WebApp
    { interfaceLanguage  :: Language
    , databaseConnection :: Connection
    }

type WebAppRoute = Route WebApp

mkYesod "WebApp" [parseRoutes|
/                                               HomeR GET
/manage                                         ManageR GET
/manage/add                                     AddR GET POST
/manage/edit/#Int                               EditR GET
/manage/modify/#Int/#ModifyAction/#WebAppRoute  ModifyR POST
/setting/update                                 SettingUpdateR POST
|]

instance Yesod WebApp

instance RenderMessage WebApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance PathPiece (Route WebApp) where
    fromPathPiece = parseRoute . read . unpack . decodeText
    toPathPiece = encodeText . Text.show . renderRoute

defaultWebAppLayout :: Widget -> Handler Html
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
