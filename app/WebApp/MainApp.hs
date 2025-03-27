module WebApp.MainApp (mainApp) where

import           AppConfig              (AppConfig (..))
import           Control.Monad          (forM_)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (pack)
import           Database               (getAllItems)
import           Database.SQLite.Simple (Connection)
import           Item                   (ItemField (..))
import           Localisation           (AppTitle (..), ButtonLabel (..),
                                         Label (..), Localisable (..))
import           Lucid
import qualified Network.HTTP.Types     as HTypes
import qualified Network.Wai            as Wai
import           Text.Printf            (printf)
import           WebApp                 (renderWebApp)

mainApp :: AppConfig -> Connection -> Wai.Application
mainApp appConfig database _ send = do
    appHtml <- mainAppHtml appConfig database
    send $ Wai.responseBuilder HTypes.status200 [] (renderWebApp appConfig appHtml)

mainAppHtml :: AppConfig -> Connection -> IO (Html ())
mainAppHtml appConfig database = do
    let language = webInterfaceLanguage appConfig

    items <- getAllItems database

    return $ do
        div_ [class_ "mainAppHeader"] $ do
            localiseHtml AppTitle language
            a_ [class_ "button noVerticalMargin", href_ "/manage", style_ "float: right;"] (localiseHtml ManageButtonLabel language)
        div_ [class_"orderOptions"] $ do
            div_ [class_ "shoppingListCheckbox smaller", style_ "float: left; margin: calc((5vw - 4vw) / 2);"] $ do
                input_ [type_ "checkbox", id_ "hideDoneItems"]
                label_ [for_ "hideDoneItems"] ""
            label_ [class_ "centredText", for_ "hideDoneItems"] (localiseHtml HideDoneItemsLabel language)
        div_ [class_ "shoppingList"] $
            table_ [] $ do
                tr_ [] $ do
                    th_ [style_ "width: 2.5em;"] (localiseHtml DoneLabel language)
                    th_ [] (localiseHtml NameLabel language)
                    th_ [style_ "width: 4em;"] (localiseHtml AmountLabel language)
                    th_ [style_ "width: 4.5em;"] (localiseHtml PriorityLabel language)
                    th_ [style_ "width: 8em;"] (localiseHtml NotesLabel language)
                forM_ items $ \item ->
                    tr_ [] $ do
                        let clickScript = printf
                                "window.location.replace('/modify?op=update&id=%d&is_finished=%s&after='+encodeURIComponent(window.location.href)+'&n='+new Date().getTime());"
                                (itemId item)
                                (show $ not $ itemIsFinished item)

                        td_ [class_ "centreAlign"] $
                            div_ [class_ "shoppingListCheckbox", onclick_ (pack clickScript)] $ do
                                let checkboxId = pack $ printf "cb%d" (itemId item)
                                input_ ([type_ "checkbox", id_ checkboxId] ++ [checked_ | itemIsFinished item])
                                label_ [for_ checkboxId] ""
                        td_ [class_ "leftAlign"] (toHtml $ itemName item)
                        td_ [class_ "centreAlign"] (toHtml $ show $ itemAmount item)
                        td_ [class_ "centreAlign"] (localiseHtml (itemPriority item) language)
                        td_ [class_ "leftAlign"] (toHtml $ fromMaybe "" (itemNotes item))
