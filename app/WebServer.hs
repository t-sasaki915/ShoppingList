{-# LANGUAGE TemplateHaskell #-}

module WebServer (startWebServer) where

import           AppConfig                (AppConfig (serverPort))
import           Data.ByteString.Builder  (byteString, lazyByteString)
import           Data.FileEmbed           (embedFile)
import           Lucid                    (renderBS)
import qualified Network.HTTP.Types       as HTypes
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Text.Printf              (printf)
import           WebApp                   (webAppHtml)

startWebServer :: AppConfig -> IO ()
startWebServer appConfig = do
    let port = serverPort appConfig
    putStrLn $ printf "The web server is listening to %d." port
    Warp.run port (router appConfig)

router :: AppConfig -> Wai.Application
router appConfig req =
    case Wai.pathInfo req of
        []            -> mainApp appConfig req
        ["style.css"] -> styleSheetApp req
        _             -> notFoundApp req

mainApp :: AppConfig -> Wai.Application
mainApp appConfig _ send = do
    webApp <- webAppHtml appConfig
    send $ Wai.responseBuilder HTypes.status200 [] (lazyByteString $ renderBS webApp)

styleSheetApp :: Wai.Application
styleSheetApp _ send =
    send $ Wai.responseBuilder HTypes.status200 [] (byteString $(embedFile "style.css"))

notFoundApp :: Wai.Application
notFoundApp _ send = send $ Wai.responseBuilder HTypes.status404 [] "NOT FOUND!"
