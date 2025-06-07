module WebApp.SettingUpdateR (getSettingUpdateR) where

import           Control.Monad.Extra (whenJust)
import           Data.Text           (Text)
import           Data.Text.TRead     (TRead (tRead))
import           Yesod

import qualified Database            as DB
import           Database.WebApp     (handleDB)
import           WebApp              (Route (HomeR), WebApp)

data SettingUpdateRequest = SettingUpdateRequest
    { maybeShouldHideDoneItems :: Maybe Bool
    , maybeItemOrder           :: Maybe Text
    } deriving Show

getSettingUpdateR :: (HandlerFor WebApp) ()
getSettingUpdateR = do
    request <- runInputGet $ SettingUpdateRequest
        <$> iopt checkBoxField "shouldHideDoneItems"
        <*> iopt textField     "itemOrder"

    whenJust (maybeShouldHideDoneItems request) $ \shouldHideDoneItems ->
        handleDB $ DB.updateShouldHideDoneItems shouldHideDoneItems

    whenJust (maybeItemOrder request) $ \itemOrder ->
        handleDB $ DB.updateItemOrder (tRead itemOrder)

    redirect HomeR
