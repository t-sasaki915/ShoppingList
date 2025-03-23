module WebApp (webAppHtml) where

import           AppConfig          (AppConfig (..))
import           Data.Version       (showVersion)
import           Localisation
import           Lucid
import           Paths_ShoppingList (version)

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
                div_ [class_ "mainAppHeader"] $ do
                    span_ [class_ "mainAppHeaderText"] (toHtml $ appTitle language)
                    span_ [class_ "mainAppHeaderText"] (toHtml $ versionLabel language)
                    span_ [class_ "mainAppHeaderText"] (toHtml $ showVersion version)
                    a_ [class_ "button", href_ "/edit", style_ "float: right;"] (toHtml $ editButtonLabel language)
