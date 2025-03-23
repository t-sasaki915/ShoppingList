{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Config.AppConfig         (AppConfig (..))
import           Config.Loader            (loadConfig)
import           Data.ByteString.Builder  (byteString)
import           Data.FileEmbed           (embedFile)
import           Data.Version             (showVersion)
import qualified Network.HTTP.Types       as HTypes
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Paths_ShoppingList       (version)
import           Text.Printf              (printf)

main :: IO ()
main = do
    putStrLn $ printf "Starting ShoppingList version %s ..." (showVersion version)

    appConfig <- loadConfig "config.yaml"

    putStrLn $ printf "The web server is listening to %d." (serverPort appConfig)
    Warp.run (serverPort appConfig) router

router :: Wai.Application
router req =
    case Wai.pathInfo req of
        []            -> indexApp req
        ["style.css"] -> styleSheetApp req
        _             -> notFoundApp req

indexApp :: Wai.Application
indexApp req send = send $ Wai.responseBuilder HTypes.status200 [] "<html><head><link rel=\"stylesheet\" href=\"style.css\"></head><body>aaa</body></html>"

styleSheetApp :: Wai.Application
styleSheetApp _ send =
    send $ Wai.responseBuilder HTypes.status200 [] (byteString $(embedFile "style.css"))

notFoundApp :: Wai.Application
notFoundApp _ send = send $ Wai.responseBuilder HTypes.status404 [] "Not Found."
