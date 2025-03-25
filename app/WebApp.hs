module WebApp
    ( renderWebApp
    , transformQuery
    , lookupQuery
    , withQuery
    ) where

import           AppConfig               (AppConfig (..))
import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder (Builder, byteString, lazyByteString)
import           Data.Functor            ((<&>))
import qualified Data.Map                as M
import           Data.Text               (Text, pack)
import           Data.Text.Encoding      (decodeUtf8Lenient, encodeUtf8)
import           Localisation            (appTitle, htmlLanguageCode)
import           Lucid
import qualified Network.HTTP.Types      as HTypes
import qualified Network.Wai             as Wai
import           Text.Printf             (printf)

constructWebApp :: AppConfig -> Html () -> Html ()
constructWebApp appConfig content = do
    let language = webInterfaceLanguage appConfig

    doctype_
    html_ [lang_ (htmlLanguageCode language)] $ do
        head_ $ do
            title_ [] (toHtml $ appTitle language)
            meta_ [charset_ "UTF-8"]
            meta_ [name_ "viewport", content_ "width=device-width,initial-scale=1"]
            link_ [rel_ "stylesheet", href_ "style.css"]
        body_ content

renderWebApp :: AppConfig -> Html () -> Builder
renderWebApp appConfig content =
    lazyByteString $ renderBS (constructWebApp appConfig content)

transformQuery :: [(ByteString, Maybe ByteString)] -> M.Map Text (Maybe Text)
transformQuery = M.fromList . map (\(k, v) -> (decodeUtf8Lenient k, v <&> decodeUtf8Lenient))

lookupQuery :: Text -> M.Map Text (Maybe Text) -> Maybe Text
lookupQuery k l =
    case M.lookup k l of
        Just (Just x) -> Just x
        _             -> Nothing

withQuery :: Text -> M.Map Text (Maybe Text) -> (Text -> IO Wai.Response) -> IO Wai.Response
withQuery q l f =
    case lookupQuery q l of
        Just x  -> f x
        Nothing ->
            return $ Wai.responseBuilder HTypes.status400 []
                (byteString (encodeUtf8 $ pack $ printf "REQUIRED QUERY '%s' IS MISSING!" q))
