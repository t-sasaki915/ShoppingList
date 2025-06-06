{-# LANGUAGE ImpredicativeTypes #-}

module WebApp.HomeR (getHomeR) where

import           Yesod

import           Localisation
import           Localisation.WebApp (localiseHandler)
import           WebApp              (WebApp (..))

getHomeR :: (HandlerFor WebApp) Html
getHomeR = do
    localiser <- localiseHandler

    defaultLayout $ do
        setTitle (localiser AppTitle)

        toWidgetHead
            [hamlet|
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width,initial-scale=1">
                <link rel="stylesheet" href="style.css">
            |]

        [whamlet|#{localiser SortOptionLabel}|]
