module WebApp.AddApp (addApp) where

import           AppConfig               (AppConfig (..))
import           Data.ByteString.Builder (byteString)
import           Database.SQLite.Simple  (Connection)
import           Item                    (ItemPriority (..))
import           Localisation
import           Lucid
import qualified Network.HTTP.Types      as HTypes
import qualified Network.Wai             as Wai
import           WebApp                  (renderWebApp)

addApp :: AppConfig -> Connection -> Wai.Application
addApp appConfig database _ send = do
    appHtml <- addAppHtml appConfig database
    send $ Wai.responseBuilder HTypes.status200 [] (byteString $ renderWebApp appConfig appHtml)

addAppHtml :: AppConfig -> Connection -> IO (Html ())
addAppHtml appConfig _ = do
    let language = webInterfaceLanguage appConfig

    return $ do
        div_ [class_ "mainAppHeader"] $ do
            span_ [class_ "mainAppHeaderText"] (toHtml $ appTitle language)
            a_ [class_ "button noVerticalMargin", href_ "#", style_ "float: right;", onclick_ "addItem();"] (toHtml $ doneButtonLabel language)
            a_ [class_ "button noVerticalMargin", href_ "/edit", style_ "float: right;"] (toHtml $ cancelButtonLabel language)
        div_ [class_ "shoppingList"] $
            table_ [] $ do
                tr_ [] $ do
                    th_ (toHtml $ nameLabel language)
                    th_ (toHtml $ amountLabel language)
                    th_ (toHtml $ priorityLabel language)
                    th_ (toHtml $ notesLabel language)
                tr_ [] $ do
                    td_ [class_ "leftAlign"] $
                        input_ [type_ "text", value_ "", class_ "itemDataInput", id_ "itemName"]
                    td_ [class_ "centreAlign"] $
                        input_ [type_ "number", value_ "1", class_ "itemDataInput", min_ "1", id_ "itemAmount"]
                    td_ [class_ "centreAlign"] $
                        select_ [class_ "itemDataInput"] $ do
                            option_ [id_ "priorityHigh"] (toHtml $ localise High language)
                            option_ [selected_ "true", id_ "priorityNormal"] (toHtml $ localise Normal language)
                            option_ [id_ "priorityLow"] (toHtml $ localise Low language)
                    td_ [class_ "leftAlign"] $
                        input_ [type_ "text", value_ "", class_ "itemDataInput", id_ "itemNotes"]
