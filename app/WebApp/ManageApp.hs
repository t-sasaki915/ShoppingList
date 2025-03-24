module WebApp.ManageApp (manageApp) where

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

manageApp :: AppConfig -> Connection -> Wai.Application
manageApp appConfig database _ send = do
    appHtml <- manageAppHtml appConfig database
    send $ Wai.responseBuilder HTypes.status200 [] (byteString $ renderWebApp appConfig appHtml)

manageAppHtml :: AppConfig -> Connection -> IO (Html ())
manageAppHtml appConfig database = do
    let language = webInterfaceLanguage appConfig

    items <- getAllItems database

    return $ do
        div_ [class_ "mainAppHeader"] $ do
            span_ [class_ "mainAppHeaderText"] (toHtml $ appTitle language)
            a_ [class_ "button noVerticalMargin", href_ "/", style_ "float: right;"] (toHtml $ viewButtonLabel language)
            a_ [class_ "button noVerticalMargin", href_ "/add", style_ "float: right;"] (toHtml $ addButtonLabel language)
        div_ [class_ "shoppingList"] $
            table_ [] $ do
                tr_ [] $ do
                    th_ [] (toHtml $ nameLabel language)
                    th_ [style_ "width: 4em;"] (toHtml $ amountLabel language)
                    th_ [style_ "width: 4.5em;"] (toHtml $ priorityLabel language)
                    th_ [] (toHtml $ notesLabel language)
                    th_ [style_ "width: 5em;"] (toHtml $ operationLabel language)
                forM_ items $ \item ->
                    tr_ [] $ do
                        td_ [class_ "leftAlign"] (toHtml $ itemName item)
                        td_ [class_ "centreAlign"] (toHtml $ show $ itemAmount item)
                        td_ [class_ "centreAlign"] (toHtml $ localise (itemPriority item) language)
                        td_ [class_ "leftAlign"] (toHtml $ fromMaybe "" (itemNotes item))
                        td_ [class_ "centreAlign"] $ do
                            let delScript = printf
                                    "window.location.replace('/modify?op=delete&id=%d&after='+encodeURIComponent(window.location.href)+'&n='+new Date().getTime());"
                                    (itemId item)

                            a_ [class_ "button noHorizontalMargin", href_ "#"] (toHtml $ editButtonLabel language)
                            br_ []
                            a_ [class_ "button noHorizontalMargin", href_ "#", onclick_ (pack delScript)] (toHtml $ deleteButtonLabel language)
