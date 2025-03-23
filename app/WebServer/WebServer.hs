{-# LANGUAGE TemplateHaskell #-}

module WebServer.WebServer (startWebServer) where

import           Config.AppConfig         (AppConfig (serverPort))
import           Data.ByteString.Builder  (byteString)
import           Data.FileEmbed           (embedFile)
import qualified Network.HTTP.Types       as HTypes
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Text.Printf              (printf)

startWebServer :: AppConfig -> IO ()
startWebServer appConfig = do
    let port = serverPort appConfig
    putStrLn $ printf "The web server is listening to %d." port
    Warp.run port router

router :: Wai.Application
router req =
    case Wai.pathInfo req of
        []            -> mainApp req
        ["style.css"] -> styleSheetApp req
        _             -> notFoundApp req

mainApp :: Wai.Application
mainApp req send = send $ Wai.responseBuilder HTypes.status200 [] "<html><head><link rel=\"stylesheet\" href=\"style.css\"></head><body>aaa</body></html>"

styleSheetApp :: Wai.Application
styleSheetApp _ send =
    send $ Wai.responseBuilder HTypes.status200 [] (byteString $(embedFile "style.css"))

notFoundApp :: Wai.Application
notFoundApp _ send = send $ Wai.responseBuilder HTypes.status404 [] "NOT FOUND!"
