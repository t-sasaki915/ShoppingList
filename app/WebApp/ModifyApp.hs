module WebApp.ModifyApp (modifyApp) where

import           AppConfig              (AppConfig (..))
import           Control.Monad          (forM_)
import           Data.Functor           ((<&>))
import           Data.Maybe             (fromJust)
import           Data.Text              (unpack)
import           Data.Text.TRead        (TRead (..))
import qualified Database               as DB
import           Database.SQLite.Simple (Connection)
import qualified Network.HTTP.Types     as HTypes
import           Network.URI            (parseURI)
import qualified Network.URI.Encode     as URI
import qualified Network.Wai            as Wai
import           Network.Wai.Util       (redirect')
import           WebApp                 (lookupQuery, transformQuery, withQuery)

modifyApp :: AppConfig -> Connection -> Wai.Application
modifyApp _ database req send = do
    let queryMap = transformQuery $ Wai.queryString req

    res <- withQuery "after" queryMap $ \afterUrl ->
        withQuery "op" queryMap $ \case
            "update" ->
                withQuery "id" queryMap $ \rawId -> do
                    let iid = tRead rawId
                        newName = lookupQuery "name" queryMap <&> URI.decodeText
                        newAmount = tReadMaybe =<< lookupQuery "amount" queryMap
                        newPriority = tReadMaybe =<< lookupQuery "priority" queryMap
                        newNotes = lookupQuery "notes" queryMap <&> URI.decodeText
                        newIsFinished = tReadMaybe =<< lookupQuery "is_finished" queryMap

                    forM_ newName (DB.updateItemName database iid)
                    forM_ newAmount (DB.updateItemAmount database iid)
                    forM_ newPriority (DB.updateItemPriority database iid)
                    forM_ newNotes (DB.updateItemNotes database iid)
                    forM_ newIsFinished (DB.updateItemIsFinished database iid)

                    redirect afterUrl

            "delete" ->
                withQuery "id" queryMap $ \rawId -> do
                    let iid = tRead rawId

                    DB.deleteItem database iid

                    redirect afterUrl

            "add" ->
                withQuery "name" queryMap $ \newName ->
                    withQuery "amount" queryMap $ \newAmount ->
                        withQuery "priority" queryMap $ \newPriority ->
                            withQuery "notes" queryMap $ \newNotes -> do
                                DB.addItem database (URI.decodeText newName) (tRead newAmount) (tRead newPriority) (URI.decodeText newNotes)

                                redirect afterUrl

            "setting" -> do
                let newShouldHideDoneItems = tReadMaybe =<< lookupQuery "hide_done_items" queryMap

                forM_ newShouldHideDoneItems (DB.updateShouldHideDoneItems database)

                redirect afterUrl

            _ ->
                return $ Wai.responseBuilder HTypes.status400 [] "INVALID REQUEST!"

    send res

    where
        redirect afterUrl = redirect' HTypes.status301 [] (fromJust $ parseURI (URI.decode $ unpack afterUrl))
