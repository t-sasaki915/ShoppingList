module WebApp.AddApp (addApp) where

import           AppConfig              (AppConfig (..))
import           Data.Text              (pack)
import           Database.SQLite.Simple (Connection)
import           Item                   (ItemPriority (..))
import           Localisation           (AppTitle (..), ButtonLabel (..),
                                         Label (..), Localisable (..))
import           Lucid
import qualified Network.HTTP.Types     as HTypes
import qualified Network.Wai            as Wai
import           WebApp                 (renderWebApp)

addApp :: AppConfig -> Connection -> Wai.Application
addApp appConfig database _ send = do
    appHtml <- addAppHtml appConfig database
    send $ Wai.responseBuilder HTypes.status200 [] (renderWebApp appConfig appHtml)

addAppHtml :: AppConfig -> Connection -> IO (Html ())
addAppHtml appConfig _ = do
    let language = webInterfaceLanguage appConfig

    return $ do
        script_ $ pack $ unlines
            [ "function addItem() {"
            , "  const itemName = encodeURIComponent(document.getElementById('itemName').value);"
            , "  const itemAmount = document.getElementById('itemAmount').value;"
            , "  const isPriorityHigh = document.getElementById('priorityHigh').selected;"
            , "  const isPriorityNormal = document.getElementById('priorityNormal').selected;"
            , "  const isPriorityLow = document.getElementById('priorityLow').selected;"
            , "  const itemNotes = encodeURIComponent(document.getElementById('itemNotes').value);"
            , "  const priorityText = isPriorityHigh ? 'High' : isPriorityNormal ? 'Normal' : isPriorityLow ? 'Low' : '';"
            , "  const manageAppUrl = encodeURIComponent(`${window.location.origin}/manage`);"
            , "  const time = new Date().getTime();"
            , "  const url = `/modify?op=add&name=${itemName}&amount=${itemAmount}&priority=${priorityText}&notes=${itemNotes}&after=${manageAppUrl}&n=${time}`;"
            , "  window.location.replace(url);"
            , "}"
            ]
        div_ [class_ "mainAppHeader"] $ do
            localiseHtml AppTitle language
            a_ [class_ "button noVerticalMargin", href_ "#", style_ "float: right;", onclick_ "addItem();"] (localiseHtml DoneButtonLabel language)
            a_ [class_ "button noVerticalMargin", href_ "/manage", style_ "float: right;"] (localiseHtml CancelButtonLabel language)
        div_ [class_ "shoppingList"] $
            table_ [] $ do
                tr_ [] $ do
                    th_ [] (localiseHtml NameLabel language)
                    th_ [style_ "width: 5em;"] (localiseHtml AmountLabel language)
                    th_ [style_ "width: 5em;"] (localiseHtml PriorityLabel language)
                    th_ [style_ "width: 9em;"] (localiseHtml NotesLabel language)
                tr_ [] $ do
                    td_ [class_ "leftAlign"] $
                        input_ [type_ "text", value_ "", class_ "itemDataInput", id_ "itemName"]
                    td_ [class_ "centreAlign"] $
                        input_ [type_ "number", value_ "1", class_ "itemDataInput", min_ "1", id_ "itemAmount"]
                    td_ [class_ "centreAlign"] $
                        select_ [class_ "itemDataInput"] $ do
                            option_ [id_ "priorityHigh"] (localiseHtml High language)
                            option_ [selected_ "true", id_ "priorityNormal"] (localiseHtml Normal language)
                            option_ [id_ "priorityLow"] (localiseHtml Low language)
                    td_ [class_ "leftAlign"] $
                        input_ [type_ "text", value_ "", class_ "itemDataInput", id_ "itemNotes"]
