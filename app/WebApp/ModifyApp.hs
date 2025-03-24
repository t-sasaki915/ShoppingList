module WebApp.ModifyApp (modifyApp) where

import           AppConfig              (AppConfig (..))
import           Control.Monad          (forM_)
import           Data.ByteString        (ByteString)
import           Data.Functor           ((<&>))
import qualified Data.Map               as M
import           Data.Maybe             (fromJust)
import           Data.Text              (Text, unpack)
import           Data.Text.Encoding     (decodeUtf8Lenient)
import           Data.Text.TRead        (TRead (..))
import           Data.Yaml.Internal     (isNumeric)
import qualified Database               as DB
import           Database.SQLite.Simple (Connection)
import qualified Network.HTTP.Types     as HTypes
import           Network.URI            (parseURI)
import qualified Network.URI.Encode     as URI
import qualified Network.Wai            as Wai
import           Network.Wai.Util       (redirect')

modifyApp :: AppConfig -> Connection -> Wai.Application
modifyApp _ database req send = do
    let queryMap = transform $ Wai.queryString req

    case lookup' "after" queryMap of
        Just afterUrl ->
            case lookup' "op" queryMap of
                Just "update" ->
                    case lookup' "id" queryMap of
                        Just x | isNumeric x -> do
                            let iid = tRead x
                                newName = lookup' "name" queryMap <&> URI.decodeText
                                newAmount = tReadMaybe =<< lookup' "amount" queryMap
                                newPriority = tReadMaybe =<< lookup' "priority" queryMap
                                newNotes = lookup' "notes" queryMap <&> URI.decodeText
                                newIsFinished = tReadMaybe =<< lookup' "is_finished" queryMap

                            forM_ newName (DB.updateItemName database iid)
                            forM_ newAmount (DB.updateItemAmount database iid)
                            forM_ newPriority (DB.updateItemPriority database iid)
                            forM_ newNotes (DB.updateItemNotes database iid)
                            forM_ newIsFinished (DB.updateItemIsFinished database iid)

                            send =<< redirect' HTypes.status308 [] (fromJust $ parseURI (URI.decode $ unpack afterUrl))

                        _ ->
                            invalidRequest

                _ ->
                    invalidRequest

        _ ->
            invalidRequest
    where
        transform :: [(ByteString, Maybe ByteString)] -> M.Map Text (Maybe Text)
        transform = M.fromList . map (\(key, mvalue) -> (decodeUtf8Lenient key, mvalue <&> decodeUtf8Lenient))

        lookup' :: Text -> M.Map Text (Maybe Text) -> Maybe Text
        lookup' key lst =
            case M.lookup key lst of
                Just (Just x) -> Just x
                _             -> Nothing

        invalidRequest = send $ Wai.responseBuilder HTypes.status400 [] "INVALID REQUEST!"
