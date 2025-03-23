module AppConfig
    ( AppConfig(..)
    , defaultAppConfig
    ) where

import qualified AppConfig.Resource as Res
import           Data.Yaml

newtype AppConfig = AppConfig
    { serverPort :: Int
    }
    deriving Show

defaultAppConfig :: AppConfig
defaultAppConfig =
    AppConfig
        { serverPort = Res.defaultServerPort
        }

instance FromJSON AppConfig where
    parseJSON (Object m) =
        AppConfig
            <$> m .:? "serverPort" .!= Res.defaultServerPort

    parseJSON Null = return defaultAppConfig

    parseJSON _ = fail "Unrecognisable config"
