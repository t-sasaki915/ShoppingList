module WebApp.ModifyR (ModifyAction (..), postModifyR) where

import           Control.Monad       (when)
import           Control.Monad.Extra (whenJust)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.TRead     (TRead (..))
import           Yesod

import qualified Database            as DB
import           Database.WebApp     (handleDB)
import           WebApp              (WebApp)

data ModifyAction = DeleteItem
                  | UpdateItem
                  deriving (Show, Eq, Read)

instance TRead ModifyAction where
    tReadMaybe "DeleteItem" = Just DeleteItem
    tReadMaybe "UpdateItem" = Just UpdateItem
    tReadMaybe _            = Nothing

instance PathPiece ModifyAction where
    fromPathPiece = tReadMaybe
    toPathPiece = Text.show

data UpdateItemRequest = UpdateItemRequest
    { maybeItemName         :: Maybe Text
    , maybeItemAmount       :: Maybe Int
    , maybeItemPriority     :: Maybe Text
    , maybeItemNotes        :: Maybe Text
    , maybeItemIsFinished   :: Maybe Bool
    , shouldUpdateItemNotes :: Maybe Bool
    } deriving Show

postModifyR :: Int -> ModifyAction -> Route WebApp -> (HandlerFor WebApp) ()
postModifyR itemId DeleteItem afterModify = do
    handleDB $ DB.deleteItem itemId

    redirect afterModify

postModifyR itemId UpdateItem afterModify = do
    request <- runInputPost $ UpdateItemRequest
        <$> iopt textField     "itemName"
        <*> iopt intField      "itemAmount"
        <*> iopt textField     "itemPriority"
        <*> iopt textField     "itemNotes"
        <*> iopt checkBoxField "itemIsFinished"
        <*> iopt hiddenField   "shouldUpdateItemNotes"

    whenJust (maybeItemName request) $ \itemName ->
        handleDB $ DB.updateItemName itemId itemName

    whenJust (maybeItemAmount request) $ \itemAmount ->
        handleDB $ DB.updateItemAmount itemId itemAmount

    whenJust (maybeItemPriority request) $ \itemPriority ->
        handleDB $ DB.updateItemPriority itemId (tRead itemPriority)

    whenJust (maybeItemIsFinished request) $ \itemIsFinished ->
        handleDB $ DB.updateItemIsFinished itemId itemIsFinished

    when (fromMaybe False (shouldUpdateItemNotes request)) $
        handleDB $ DB.updateItemNotes itemId (maybeItemNotes request)

    redirect afterModify
