module WebServer (startWebServer) where

import           AppConfig                (AppConfig (serverPort))
import           Data.ByteString.Builder  (byteString)
import           Database.SQLite.Simple   (Connection)
import qualified Network.HTTP.Types       as HTypes
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Text.Printf              (printf)
import           WebApp.AddApp            (addApp)
import           WebApp.EditApp           (editApp)
import           WebApp.MainApp           (mainApp)
import           WebApp.ManageApp         (manageApp)
import           WebApp.ModifyApp         (modifyApp)
import qualified WebServer.Resource       as Res

startWebServer :: AppConfig -> Connection -> IO ()
startWebServer appConfig database = do
    let port = serverPort appConfig
    putStrLn $ printf "The web server is listening to %d." port
    Warp.run port (router appConfig database)

router :: AppConfig -> Connection -> Wai.Application
router appConfig database req =
    case Wai.pathInfo req of
        []            -> mainApp appConfig database req
        ["add"]       -> addApp appConfig database req
        ["edit"]      -> editApp appConfig database req
        ["manage"]    -> manageApp appConfig database req
        ["modify"]    -> modifyApp appConfig database req
        ["style.css"] -> styleSheetApp req
        _             -> notFoundApp req

styleSheetApp :: Wai.Application
styleSheetApp _ send =
    send $ Wai.responseBuilder HTypes.status200 [] (byteString Res.styleSheetFile)

notFoundApp :: Wai.Application
notFoundApp _ send = send $ Wai.responseBuilder HTypes.status404 [] "NOT FOUND!"
