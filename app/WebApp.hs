{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module WebApp
    ( WebApp (..)
    , initialiseWebApp
    ) where

import                          Database.SQLite.Simple (Connection)
import                          Yesod

import                          AppConfig              (AppConfig (..))
import                          Localisation           (Language)
import {-# SOURCE #-}           WebApp.HomeR           (getHomeR)

data WebApp = WebApp
    { interfaceLanguage  :: Language
    , databaseConnection :: Connection
    }

mkYesod "WebApp" [parseRoutes|
/ HomeR GET
|]

instance Yesod WebApp

initialiseWebApp :: AppConfig -> Connection -> WebApp
initialiseWebApp appConfig dbConnection =
    WebApp
        { interfaceLanguage  = webInterfaceLanguage appConfig
        , databaseConnection = dbConnection
        }
