module WebApp.AddR (getAddR, postAddR) where

import                          Yesod  (HandlerFor, Html)

import {-# SOURCE #-}           WebApp (WebApp)

getAddR :: (HandlerFor WebApp) Html

postAddR :: (HandlerFor WebApp) ()
