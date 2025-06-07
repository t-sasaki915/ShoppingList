module WebApp.ModifyR (ModifyAction (..), postModifyR) where

import qualified Data.Text       as Text
import           Data.Text.TRead (TRead (..))
import           Yesod

import qualified Database        as DB
import           Database.WebApp (handleDB)
import           WebApp          (WebApp)

data ModifyAction = DeleteItem
                  deriving (Show, Eq, Read)

instance TRead ModifyAction where
    tReadMaybe "DeleteItem" = Just DeleteItem
    tReadMaybe _            = Nothing

instance PathPiece ModifyAction where
    fromPathPiece = tReadMaybe
    toPathPiece = Text.show

postModifyR :: Int -> ModifyAction -> Route WebApp -> (HandlerFor WebApp) ()
postModifyR itemId DeleteItem afterModify = do
    handleDB $ DB.deleteItem itemId

    redirect afterModify
