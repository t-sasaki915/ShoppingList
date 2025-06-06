module WebApp.OrderOptionUpdateR (getOrderOptionUpdateR) where

import           Data.Text       (Text)
import           Data.Text.TRead (TRead (tRead))
import           Yesod

import qualified Database        as DB
import           Database.WebApp (handleDB)
import           WebApp          (Route (HomeR), WebApp)

data OrderOptionUpdateRequest = OrderOptionUpdateRequest
    { hideDoneItems :: Bool
    , sortOption    :: Text
    } deriving Show

getOrderOptionUpdateR :: (HandlerFor WebApp) ()
getOrderOptionUpdateR = do
    request <- runInputGet $ OrderOptionUpdateRequest
        <$> ireq checkBoxField "hideDoneItems"
        <*> ireq textField     "sortOption"

    handleDB $ DB.updateShouldHideDoneItems (hideDoneItems request)
    handleDB $ DB.updateItemOrder (tRead $ sortOption request)

    redirect HomeR
