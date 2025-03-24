module WebApp.EditApp (editApp) where

import           AppConfig               (AppConfig (..))
import           Data.ByteString.Builder (byteString)
import           Database.SQLite.Simple  (Connection)
import           Localisation
import           Lucid
import qualified Network.HTTP.Types      as HTypes
import qualified Network.Wai             as Wai
import           WebApp                  (renderWebApp)

editApp :: AppConfig -> Connection -> Wai.Application
editApp appConfig database _ send = do
    appHtml <- editAppHtml appConfig database
    send $ Wai.responseBuilder HTypes.status200 [] (byteString $ renderWebApp appConfig appHtml)

editAppHtml :: AppConfig -> Connection -> IO (Html ())
editAppHtml appConfig _ = do
    let language = webInterfaceLanguage appConfig

    return $ do
        div_ [class_ "mainAppHeader"] $ do
            span_ [class_ "mainAppHeaderText"] (toHtml $ appTitle language)
            a_ [class_ "button", href_ "/", style_ "float: right;"] (toHtml $ viewButtonLabel language)
