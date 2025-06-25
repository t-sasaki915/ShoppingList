{-# LANGUAGE ImpredicativeTypes #-}

module Localisation.WebApp (localiseHandler) where

import           Data.Functor ((<&>))
import           Yesod        (HandlerFor, Html, getYesod, toHtml)

import           Localisation (Localisable (..))
import           WebApp       (WebApp (interfaceLanguage))

localiseHandler :: (HandlerFor WebApp) (forall a. Localisable a => a -> Html)
localiseHandler = getYesod <&> interfaceLanguage >>= \lang -> pure $ toHtml . flip localise lang
