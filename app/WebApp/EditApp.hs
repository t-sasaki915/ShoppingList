module WebApp.EditApp (editApp) where

import           AppConfig               (AppConfig (..))
import           Data.ByteString.Builder (byteString)
import           Data.Maybe              (fromMaybe)
import           Data.Text               (pack)
import           Data.Text.TRead         (TRead (tRead))
import           Data.Yaml.Internal      (isNumeric)
import           Database                (getItem)
import           Database.SQLite.Simple  (Connection)
import           Item                    (ItemField (..), ItemPriority (..))
import           Localisation
import           Lucid
import qualified Network.HTTP.Types      as HTypes
import qualified Network.Wai             as Wai
import           WebApp                  (lookupQuery, renderWebApp,
                                          transformQuery)

editApp :: AppConfig -> Connection -> Wai.Application
editApp appConfig database req send = do
    let queryMap = transformQuery $ Wai.queryString req

    case lookupQuery "id" queryMap of
        Just x | isNumeric x -> do
            let iid = tRead x

            appHtml <- editAppHtml appConfig database iid
            send $ Wai.responseBuilder HTypes.status200 [] (byteString $ renderWebApp appConfig appHtml)

        _ ->
            send $ Wai.responseBuilder HTypes.status400 [] "INVALID REQUEST!"

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
            span_ [class_ "mainAppHeaderText"] (toHtml $ appTitle language)
            a_ [class_ "button noVerticalMargin", href_ "#", style_ "float: right;", onclick_ "apply();"] (toHtml $ doneButtonLabel language)
            a_ [class_ "button noVerticalMargin", href_ "/manage", style_ "float: right;"] (toHtml $ cancelButtonLabel language)
        div_ [class_ "shoppingList"] $
            table_ [] $ do
                tr_ [] $ do
                    th_ [] (toHtml $ nameLabel language)
                    th_ [style_ "width: 5em;"] (toHtml $ amountLabel language)
                    th_ [style_ "width: 5em;"] (toHtml $ priorityLabel language)
                    th_ [style_ "width: 9em;"] (toHtml $ notesLabel language)
                tr_ [] $ do
                    td_ [class_ "leftAlign"] $
                        input_ [type_ "text", value_ (itemName item), class_ "itemDataInput", id_ "itemName"]
                    td_ [class_ "centreAlign"] $
                        input_ [type_ "number", value_ (pack $ show $ itemAmount item), class_ "itemDataInput", min_ "1", id_ "itemAmount"]
                    td_ [class_ "centreAlign"] $
                        select_ [class_ "itemDataInput"] $ do
                            let p = itemPriority item

                            option_ (id_ "priorityHigh" : [selected_ "true" | p == High]) (toHtml $ localise High language)
                            option_ (id_ "priorityNormal" : [selected_ "true" | p == Normal]) (toHtml $ localise Normal language)
                            option_ (id_ "priorityLow" : [selected_ "true" | p == Low]) (toHtml $ localise Low language)
                    td_ [class_ "leftAlign"] $
                        input_ [type_ "text", value_ (fromMaybe "" (itemNotes item)), class_ "itemDataInput", id_ "itemNotes"]
