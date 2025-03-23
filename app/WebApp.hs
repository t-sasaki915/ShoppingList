module WebApp (renderWebApp) where

import           AppConfig       (AppConfig (..))
import           Data.ByteString (ByteString, toStrict)
import           Localisation
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
