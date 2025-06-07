module WebApp.EditR (getEditR) where

import                          Yesod  (HandlerFor, Html)

import {-# SOURCE #-}           WebApp (WebApp)

getEditR :: Int -> (HandlerFor WebApp) Html
