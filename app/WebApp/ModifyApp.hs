module WebApp.ModifyApp (modifyApp) where

import           AppConfig              (AppConfig (..))
import           Control.Monad          (forM_)
import           Data.Functor           ((<&>))
import           Data.Maybe             (fromJust, isNothing)
import           Data.Text              (unpack)
import           Data.Text.TRead        (TRead (..))
import           Data.Yaml.Internal     (isNumeric)
import qualified Database               as DB
import           Database.SQLite.Simple (Connection)
import qualified Network.HTTP.Types     as HTypes
import           Network.URI            (parseURI)
import qualified Network.URI.Encode     as URI
import qualified Network.Wai            as Wai
import           Network.Wai.Util       (redirect')
import           WebApp                 (lookupQuery, transformQuery)

modifyApp :: AppConfig -> Connection -> Wai.Application
modifyApp _ database req send = do
    let queryMap = transformQuery $ Wai.queryString req

    case lookupQuery "after" queryMap of
        Just afterUrl ->
            case lookupQuery "op" queryMap of
                Just "update" ->
                    case lookupQuery "id" queryMap of
                        Just x | isNumeric x -> do
                            let iid = tRead x
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

                        _ ->
                            invalidRequest

                Just "delete" ->
                    case lookupQuery "id" queryMap of
                        Just x | isNumeric x -> do
                            let iid = tRead x

                            DB.deleteItem database iid

                            redirect afterUrl

                        _ ->
                            invalidRequest

                Just "add" -> do
                    let newName = lookupQuery "name" queryMap <&> URI.decodeText
                        newAmount = tReadMaybe =<< lookupQuery "amount" queryMap
                        newPriority = tReadMaybe =<< lookupQuery "priority" queryMap
                        newNotes = lookupQuery "notes" queryMap <&> URI.decodeText

                    if isNothing newName || isNothing newAmount || isNothing newPriority || isNothing newNotes then
                        invalidRequest

                    else do
                        DB.addItem database (fromJust newName) (fromJust newAmount) (fromJust newPriority) (fromJust newNotes)

                        redirect afterUrl

                _ ->
                    invalidRequest

        _ ->
            invalidRequest
    where
        invalidRequest = send $ Wai.responseBuilder HTypes.status400 [] "INVALID REQUEST!"
        redirect afterUrl = send =<< redirect' HTypes.status301 [] (fromJust $ parseURI (URI.decode $ unpack afterUrl))
