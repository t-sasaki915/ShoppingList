module WebApp.ManageR (getManageR) where

import                          Yesod  (HandlerFor, Html)

import {-# SOURCE #-}           WebApp (WebApp)

getManageR :: (HandlerFor WebApp) Html
