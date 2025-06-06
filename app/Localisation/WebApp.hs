{-# LANGUAGE ImpredicativeTypes #-}

module Localisation.WebApp (localiseHandler) where

import           Yesod        (HandlerFor, Html, getYesod, toHtml)

import           Localisation (Localisable (..))
import           WebApp       (WebApp (interfaceLanguage))

localiseHandler :: (HandlerFor WebApp) (forall a. Localisable a => a -> Html)
localiseHandler = getYesod >>= (\lang -> return $ toHtml . flip localise lang) . interfaceLanguage
