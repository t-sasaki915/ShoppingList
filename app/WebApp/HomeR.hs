module WebApp.HomeR (getHomeR) where

import           Data.Functor ((<&>))
import           Yesod

import           Localisation
import           WebApp       (WebApp (..))

getHomeR :: (HandlerFor WebApp) Html
getHomeR = do
    interfaceLang <- getYesod <&> interfaceLanguage

    defaultLayout $ do
        setTitle (localiseHtml AppTitle interfaceLang)

        toWidgetHead
            [hamlet|
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width,initial-scale=1">
                <link rel="stylesheet" href="style.css">
                aaaaa!!
            |]
