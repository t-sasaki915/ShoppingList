module WebApp.ModifyR (ModifyAction, postModifyR) where

import                          Yesod  (HandlerFor, PathPiece, Route)

import {-# SOURCE #-}           WebApp (WebApp)

data ModifyAction

instance Eq ModifyAction
instance Show ModifyAction
instance Read ModifyAction
instance PathPiece ModifyAction

postModifyR :: Int -> ModifyAction -> Route WebApp -> (HandlerFor WebApp) ()

