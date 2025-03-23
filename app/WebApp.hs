module WebApp (webAppHtml) where

import           AppConfig    (AppConfig (..))
import           Localisation
import           Lucid

webAppHtml :: AppConfig -> IO (Html ())
webAppHtml appConfig = do
    let language = webInterfaceLanguage appConfig

    return $ do
        doctype_
        html_ [lang_ (htmlLanguageCode language)] $ do
            head_ $ do
                title_ [] (toHtml $ appTitle language)
                meta_ [charset_ "UTF-8"]
                meta_ [name_ "viewport", content_ "width=device-width,initial-scale=1"]
                link_ [rel_ "stylesheet", href_ "style.css"]
            body_ $ do
                p_ "HELLO"
