{-# LANGUAGE ImpredicativeTypes #-}

module WebApp.HomeR (getHomeR) where

import           Yesod

import qualified Database            as DB
import           Database.WebApp     (handleDB)
import           Item                (pickAndSortItems)
import           Localisation
import           Localisation.WebApp (localiseHandler)
import           WebApp              (WebApp (..), defaultWebAppLayout)

getHomeR :: (HandlerFor WebApp) Html
getHomeR = do
    localiser <- localiseHandler

    allItems  <- handleDB DB.getAllItems
    orderOpts <- handleDB DB.getItemOrderOption

    let itemsToShow = pickAndSortItems orderOpts allItems

    defaultWebAppLayout $ do
        [whamlet|
            <div .mainAppHeader>
                #{localiser AppTitle}
                <a .button .noVerticalMargin href="/manage" style="float: right;">
                    #{localiser ManageButtonLabel}
                \
            \
        |]
