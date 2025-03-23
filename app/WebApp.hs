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
                div_ [class_ "mainAppHeader"] $ do
                    span_ [class_ "mainAppHeaderText"] (toHtml $ appTitle language)
                    a_ [class_ "button", href_ "/edit", style_ "float: right;"] (toHtml $ editButtonLabel language)
                div_ [class_ "shoppingList"] $
                    table_ [] $ do
                        tr_ [] $ do
                            th_ (toHtml $ finishedLabel language)
                            th_ (toHtml $ nameLabel language)
                            th_ (toHtml $ amountLabel language)
                            th_ (toHtml $ priorityLabel language)
                            th_ (toHtml $ notesLabel language)
