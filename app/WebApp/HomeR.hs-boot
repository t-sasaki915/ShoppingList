module WebApp.HomeR (getHomeR) where

import                          Yesod  (HandlerFor, Html)

import {-# SOURCE #-}           WebApp (WebApp)

getHomeR :: (HandlerFor WebApp) Html
