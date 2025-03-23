module WebApp.ModifyApp (modifyApp) where

import           AppConfig              (AppConfig (..))
import           Control.Monad          (forM_)
import           Data.Either.Extra      (fromRight')
import           Data.Functor           ((<&>))
import qualified Data.Map               as M
import           Data.Maybe             (fromJust)
import           Data.Text              (Text, unpack)
import           Data.Text.Encoding     (decodeUtf8Lenient)
import           Data.Text.Read         (decimal)
import           Data.Yaml.Internal     (isNumeric)
import           Database               (ItemPriority (..))
import qualified Database               as DB
import           Database.SQLite.Simple (Connection)
import qualified Network.HTTP.Types     as HTypes
import           Network.URI            (parseURI)
import qualified Network.Wai            as Wai
import           Network.Wai.Util       (redirect')

modifyApp :: AppConfig -> Connection -> Wai.Application
modifyApp _ database req send = do
    let requestMap = process $ Wai.queryString req

    case lookup' "after" requestMap of
        Just afterUrl ->
            case lookup' "op" requestMap of
                Just "update" ->
                    case lookup' "id" requestMap of
                        Just x | isNumeric x -> do
                            let iid = fromJust (textToInt x)
                                newName = lookup' "name" requestMap
                                newAmount = textToInt =<< lookup' "amount" requestMap
                                newPriority = textToPriority =<< lookup' "priority" requestMap
                                newNotes = lookup' "notes" requestMap
                                newIsFinished = textToBool =<< lookup' "is_finished" requestMap

                            forM_ newName (DB.updateItemName database iid)
                            forM_ newAmount (DB.updateItemAmount database iid)
                            forM_ newPriority (DB.updateItemPriority database iid)
                            forM_ newNotes (DB.updateItemNotes database iid)
                            forM_ newIsFinished (DB.updateItemIsFinished database iid)

                            send =<< redirect' HTypes.status308 [] (fromJust $ parseURI (unpack afterUrl))

                        _ ->
                            invalidRequest

                _ ->
                    invalidRequest

        _ ->
            invalidRequest
    where
        process = M.fromList . map (\(key, mvalue) -> (decodeUtf8Lenient key, mvalue <&> decodeUtf8Lenient))

        lookup' :: Text -> M.Map Text (Maybe Text) -> Maybe Text
        lookup' key lst =
            case M.lookup key lst of
                Just (Just x) -> Just x
                _             -> Nothing

        textToInt :: Text -> Maybe Int
        textToInt txt
            | isNumeric txt = Just (fst $ fromRight' $ decimal txt)
            | otherwise     = Nothing

        textToPriority :: Text -> Maybe ItemPriority
        textToPriority "High"   = Just High
        textToPriority "Normal" = Just Normal
        textToPriority "Low"    = Just Low
        textToPriority _        = Nothing

        textToBool :: Text -> Maybe Bool
        textToBool "true"  = Just True
        textToBool "false" = Just False
        textToBool _       = Nothing

        invalidRequest = send $ Wai.responseBuilder HTypes.status400 [] "INVALID REQUEST!"
