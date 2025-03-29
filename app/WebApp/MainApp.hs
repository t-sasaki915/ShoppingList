module WebApp.MainApp (mainApp) where

import           AppConfig              (AppConfig (..))
import           Control.Monad          (forM_)
import           Data.List              (sortBy)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (pack)
import           Database               (getAllItems, getItemOrderOption)
import           Database.SQLite.Simple (Connection)
import           Item                   (ItemField (..), ItemOrder (..),
                                         ItemOrderOption (..),
                                         compareItemFieldByPriority)
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
    orderOpts <- getItemOrderOption database

    let itemsToShow =
            if shouldHideDoneItems orderOpts then
                filter (not . itemIsFinished) items
            else
                items
        sortedItems =
            case itemOrder orderOpts of
                DefaultOrder  -> itemsToShow
                PriorityOrder -> sortBy compareItemFieldByPriority itemsToShow

    return $ do
        script_ $ pack $ unlines
            [ "function orderChanged(value) {"
            , "  window.location.replace(`/modify?op=setting&item_order=${value}&after=${encodeURIComponent(window.location.href)}&n=${new Date().getTime()}`);"
            , "}"
            ]
        div_ [class_ "mainAppHeader"] $ do
            localiseHtml AppTitle language
            a_ [class_ "button noVerticalMargin", href_ "/manage", style_ "float: right;"] (localiseHtml ManageButtonLabel language)
        div_ [class_ "orderOptions"] $ do
            let clickScript = printf
                    "window.location.replace('/modify?op=setting&hide_done_items=%s&after='+encodeURIComponent(window.location.href)+'&n='+new Date().getTime());"
                    (show $ not $ shouldHideDoneItems orderOpts)

            div_ [class_ "shoppingListCheckbox smaller", style_ "float: left; margin: calc((5vw - 4vw) / 2);", onclick_ (pack clickScript)] $ do
                input_ ([type_ "checkbox", id_ "hideDoneItems"] ++ [checked_ | shouldHideDoneItems orderOpts])
                label_ [for_ "hideDoneItems"] ""
            label_ [class_ "centredText", for_ "hideDoneItems"] (localiseHtml HideDoneItemsLabel language)
            div_ [class_ "spacer"] ""
            label_ [class_ "centredText"] (localiseHtml SortOptionLabel language)
            select_ [class_ "selector", onchange_ "orderChanged(value);"] $ do
                let orderIs = (==) (itemOrder orderOpts)

                option_ (value_ "DefaultOrder" : [selected_ "" | orderIs DefaultOrder]) (localiseHtml DefaultOrder language)
                option_ (value_ "PriorityOrder" : [selected_ "" | orderIs PriorityOrder]) (localiseHtml PriorityOrder language)
        div_ [class_ "shoppingList"] $
            table_ [] $ do
                tr_ [] $ do
                    th_ [style_ "width: 2.5em;"] (localiseHtml DoneLabel language)
                    th_ [] (localiseHtml NameLabel language)
                    th_ [style_ "width: 4em;"] (localiseHtml AmountLabel language)
                    th_ [style_ "width: 4.5em;"] (localiseHtml PriorityLabel language)
                    th_ [style_ "width: 8em;"] (localiseHtml NotesLabel language)
                forM_ sortedItems $ \item ->
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
