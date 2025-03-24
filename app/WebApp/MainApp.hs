module WebApp.MainApp (mainApp) where

import           AppConfig               (AppConfig (..))
import           Control.Monad           (forM_)
import           Data.ByteString.Builder (byteString)
import           Data.Maybe              (fromMaybe)
import           Data.Text               (pack)
import           Database                (getAllItems)
import           Database.SQLite.Simple  (Connection)
import           Item                    (ItemField (..))
import           Localisation
import           Lucid
import qualified Network.HTTP.Types      as HTypes
import qualified Network.Wai             as Wai
import           Text.Printf             (printf)
import           WebApp                  (renderWebApp)

mainApp :: AppConfig -> Connection -> Wai.Application
mainApp appConfig database _ send = do
    appHtml <- mainAppHtml appConfig database
    send $ Wai.responseBuilder HTypes.status200 [] (byteString $ renderWebApp appConfig appHtml)

mainAppHtml :: AppConfig -> Connection -> IO (Html ())
mainAppHtml appConfig database = do
    let language = webInterfaceLanguage appConfig

    items <- getAllItems database

    return $ do
        div_ [class_ "mainAppHeader"] $ do
            span_ [class_ "mainAppHeaderText"] (toHtml $ appTitle language)
            a_ [class_ "button noVerticalMargin", href_ "/manage", style_ "float: right;"] (toHtml $ manageButtonLabel language)
        div_ [class_ "shoppingList"] $
            table_ [] $ do
                tr_ [] $ do
                    th_ [style_ "width: 2.5em;"] (toHtml $ finishedLabel language)
                    th_ [] (toHtml $ nameLabel language)
                    th_ [style_ "width: 4em;"] (toHtml $ amountLabel language)
                    th_ [style_ "width: 4.5em;"] (toHtml $ priorityLabel language)
                    th_ [] (toHtml $ notesLabel language)
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
                        td_ [class_ "centreAlign"] (toHtml $ localise (itemPriority item) language)
                        td_ [class_ "leftAlign"] (toHtml $ fromMaybe "" (itemNotes item))
