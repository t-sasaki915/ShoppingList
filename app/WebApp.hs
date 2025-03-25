module WebApp (renderWebApp, transformQuery, lookupQuery) where

import           AppConfig          (AppConfig (..))
import           Data.ByteString    (ByteString, toStrict)
import           Data.Functor       ((<&>))
import qualified Data.Map           as M
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8Lenient)
import           Localisation       (appTitle, htmlLanguageCode)
import           Lucid

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

renderWebApp :: AppConfig -> Html () -> ByteString
renderWebApp appConfig content = toStrict $ renderBS (constructWebApp appConfig content)

transformQuery :: [(ByteString, Maybe ByteString)] -> M.Map Text (Maybe Text)
transformQuery = M.fromList . map (\(k, v) -> (decodeUtf8Lenient k, v <&> decodeUtf8Lenient))

lookupQuery :: Text -> M.Map Text (Maybe Text) -> Maybe Text
lookupQuery k l =
    case M.lookup k l of
        Just (Just x) -> Just x
        _             -> Nothing
