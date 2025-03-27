module WebApp.EditApp (editApp) where

import           AppConfig              (AppConfig (..))
import           Data.Maybe             (fromMaybe)
import           Data.Text              (pack)
import           Data.Text.Extra        (tshow)
import           Data.Text.TRead        (TRead (..))
import           Database               (getItem)
import           Database.SQLite.Simple (Connection)
import           Item                   (ItemField (..), ItemPriority (..))
import           Localisation           (AppTitle (..), ButtonLabel (..),
                                         Label (..), Localisable (..))
import           Lucid
import qualified Network.HTTP.Types     as HTypes
import qualified Network.Wai            as Wai
import           WebApp                 (renderWebApp, transformQuery,
                                         withQuery)

editApp :: AppConfig -> Connection -> Wai.Application
editApp appConfig database req send = do
    let queryMap = transformQuery $ Wai.queryString req

    res <- withQuery "id" queryMap $ \rawId -> do
        let iid = tRead rawId

        appHtml <- editAppHtml appConfig database iid
        return $ Wai.responseBuilder HTypes.status200 [] (renderWebApp appConfig appHtml)

    send res

editAppHtml :: AppConfig -> Connection -> Int -> IO (Html ())
editAppHtml appConfig database iid = do
    let language = webInterfaceLanguage appConfig

    item <- getItem database iid

    return $ do
        script_ $ pack $ unlines
            [ "function apply() {"
            , "  const itemName = encodeURIComponent(document.getElementById('itemName').value);"
            , "  const itemAmount = document.getElementById('itemAmount').value;"
            , "  const isPriorityHigh = document.getElementById('priorityHigh').selected;"
            , "  const isPriorityNormal = document.getElementById('priorityNormal').selected;"
            , "  const isPriorityLow = document.getElementById('priorityLow').selected;"
            , "  const itemNotes = encodeURIComponent(document.getElementById('itemNotes').value);"
            , "  const priorityText = isPriorityHigh ? 'High' : isPriorityNormal ? 'Normal' : isPriorityLow ? 'Low' : '';"
            , "  const manageAppUrl = encodeURIComponent(`${window.location.origin}/manage`);"
            , "  const time = new Date().getTime();"
            , "  const url = `/modify?op=update&id=" ++ show iid ++ "&name=${itemName}&amount=${itemAmount}&priority=${priorityText}&notes=${itemNotes}&after=${manageAppUrl}&n=${time}`;"
            , "  window.location.replace(url);"
            , "}"
            ]
        div_ [class_ "mainAppHeader"] $ do
            span_ [class_ "mainAppHeaderText"] (localiseHtml AppTitle language)
            a_ [class_ "button noVerticalMargin", href_ "#", style_ "float: right;", onclick_ "apply();"] (localiseHtml DoneButtonLabel language)
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
                        input_ [type_ "text", value_ (itemName item), class_ "itemDataInput", id_ "itemName"]
                    td_ [class_ "centreAlign"] $
                        input_ [type_ "number", value_ (tshow $ itemAmount item), class_ "itemDataInput", min_ "1", id_ "itemAmount"]
                    td_ [class_ "centreAlign"] $
                        select_ [class_ "itemDataInput"] $ do
                            let p = itemPriority item

                            option_ (id_ "priorityHigh" : [selected_ "true" | p == High]) (localiseHtml High language)
                            option_ (id_ "priorityNormal" : [selected_ "true" | p == Normal]) (localiseHtml Normal language)
                            option_ (id_ "priorityLow" : [selected_ "true" | p == Low]) (localiseHtml Low language)
                    td_ [class_ "leftAlign"] $
                        input_ [type_ "text", value_ (fromMaybe "" (itemNotes item)), class_ "itemDataInput", id_ "itemNotes"]
