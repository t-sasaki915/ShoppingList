module WebApp.ManageApp (manageApp) where

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

manageApp :: AppConfig -> Connection -> Wai.Application
manageApp appConfig database _ send = do
    appHtml <- manageAppHtml appConfig database
    send $ Wai.responseBuilder HTypes.status200 [] (renderWebApp appConfig appHtml)

manageAppHtml :: AppConfig -> Connection -> IO (Html ())
manageAppHtml appConfig database = do
    let language = webInterfaceLanguage appConfig

    items <- getAllItems database

    return $ do
        div_ [class_ "mainAppHeader"] $ do
            localiseHtml AppTitle language
            a_ [class_ "button noVerticalMargin", href_ "/", style_ "float: right;"] (localiseHtml ViewButtonLabel language)
            a_ [class_ "button noVerticalMargin", href_ "/add", style_ "float: right;"] (localiseHtml AddButtonLabel language)
        div_ [class_ "shoppingList"] $
            table_ [] $ do
                tr_ [] $ do
                    th_ [] (localiseHtml NameLabel language)
                    th_ [style_ "width: 4em;"] (localiseHtml AmountLabel language)
                    th_ [style_ "width: 4.5em;"] (localiseHtml PriorityLabel language)
                    th_ [style_ "width: 7em;"] (localiseHtml NotesLabel language)
                    th_ [style_ "width: 5em;"] (localiseHtml OperationLabel language)
                forM_ items $ \item ->
                    tr_ [] $ do
                        td_ [class_ "leftAlign"] (toHtml $ itemName item)
                        td_ [class_ "centreAlign"] (toHtml $ show $ itemAmount item)
                        td_ [class_ "centreAlign"] (localiseHtml (itemPriority item) language)
                        td_ [class_ "leftAlign"] (toHtml $ fromMaybe "" (itemNotes item))
                        td_ [class_ "centreAlign"] $ do
                            let delScript = printf
                                    "window.location.replace('/modify?op=delete&id=%d&after='+encodeURIComponent(window.location.href)+'&n='+new Date().getTime());"
                                    (itemId item)

                            a_ [class_ "button noHorizontalMargin", href_ (pack $ printf "/edit?id=%d" (itemId item))] (localiseHtml EditButtonLabel language)
                            br_ []
                            a_ [class_ "button noHorizontalMargin", href_ "#", onclick_ (pack delScript)] (localiseHtml DeleteButtonLabel language)
