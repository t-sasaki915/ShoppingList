module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTypes

main :: IO ()
main = Warp.run 8080 webServer

webServer :: Wai.Application
webServer req send = send $ Wai.responseBuilder HTypes.status200 [] "aaaa"
