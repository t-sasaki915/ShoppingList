module AppConfig
    ( AppConfig(..)
    , defaultAppConfig
    ) where

import qualified AppConfig.Resource as Res
import           Data.Yaml          (FromJSON (..), Value (..), (.!=), (.:?))
import           Localisation       (Language)

data AppConfig = AppConfig
    { serverPort           :: Int
    , webInterfaceLanguage :: Language
    }
    deriving Show

defaultAppConfig :: AppConfig
defaultAppConfig =
    AppConfig
        { serverPort           = Res.defaultServerPort
        , webInterfaceLanguage = Res.defaultWebInterfaceLanguage
        }

instance FromJSON AppConfig where
    parseJSON (Object m) =
        AppConfig
            <$> m .:? "serverPort"           .!= Res.defaultServerPort
            <*> m .:? "webInterfaceLanguage" .!= Res.defaultWebInterfaceLanguage

    parseJSON Null = return defaultAppConfig

    parseJSON _ = fail "Unrecognisable config"
