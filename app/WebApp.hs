module WebApp (webAppHtml) where

import           AppConfig              (AppConfig (..))
import           Control.Monad          (forM_)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (pack)
import           Database               (ItemField (..), getAllItems)
import           Database.SQLite.Simple (Connection)
import           Localisation
import           Lucid
import           Text.Printf            (printf)

webAppHtml :: AppConfig -> Connection -> IO (Html ())
webAppHtml appConfig database = do
    let language = webInterfaceLanguage appConfig

    items <- getAllItems database

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
                        forM_ items $ \item ->
                            tr_ [] $ do
                                td_ [class_ "centreAlign"] $
                                    div_ [class_ "shoppingListCheckbox"] $ do
                                        let checkboxId = pack $ printf "cb%d" (itemId item)
                                        input_ [type_ "checkbox", id_ checkboxId]
                                        label_ [for_ checkboxId] ""
                                td_ [class_ "leftAlign"] (toHtml $ itemName item)
                                td_ [class_ "centreAlign"] (toHtml $ show $ itemAmount item)
                                td_ [class_ "centreAlign"] (toHtml $ localise (itemPriority item) language)
                                td_ [class_ "leftAlign"] (toHtml $ fromMaybe "-" (itemNotes item))
