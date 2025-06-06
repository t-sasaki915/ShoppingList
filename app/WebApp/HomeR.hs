{-# LANGUAGE ImpredicativeTypes #-}

module WebApp.HomeR (getHomeR) where

import           Data.Maybe          (fromMaybe)
import           Data.Text           (pack)
import           Text.Printf         (printf)
import           Yesod

import qualified Database            as DB
import           Database.WebApp     (handleDB)
import           Item
import           Localisation
import           Localisation.WebApp (localiseHandler)
import           WebApp              (WebApp (..), defaultWebAppLayout)

getHomeR :: (HandlerFor WebApp) Html
getHomeR = do
    localiser <- localiseHandler

    allItems  <- handleDB DB.getAllItems
    orderOpts <- handleDB DB.getItemOrderOption

    let itemsToShow = pickAndSortItems orderOpts allItems

        shouldHideDoneItems' = shouldHideDoneItems orderOpts

        isOrder = (==) (itemOrder orderOpts)
        isOrderDefault    = isOrder DefaultOrder
        isOrderByPriority = isOrder PriorityOrder

    defaultWebAppLayout $ do
        [whamlet|
            <div .mainAppHeader>
                #{localiser AppTitle}
                <a .button .noVerticalMargin href="/manage" style="float: right">
                    #{localiser ManageButtonLabel}
        |]

        [whamlet|
            <div .orderOptions>
                <div .shoppingListCheckbox .smaller style="float: left; margin: calc((5vw - 4vw) / 2)">
                    <input type="checkbox" #hideDoneItems :shouldHideDoneItems':checked>
                    <label for="hideDoneItems">
                \
                <label .centredText for="hideDoneItems">
                    #{localiser HideDoneItemsLabel}
                \
                <div .spacer>
                \
                <label .centredText>
                    #{localiser SortOptionLabel}
                \
                <select .selector>
                    <option :isOrderDefault:checked>
                        #{localiser DefaultOrder}
                    \
                    <option :isOrderByPriority:checked>
                        #{localiser PriorityOrder}
        |]

        [whamlet|
            <div .shoppingList>
                <table>
                    <tr>
                        <th style="width: 2.5em">
                            #{localiser DoneLabel}
                        \
                        <th>
                            #{localiser NameLabel}
                        \
                        <th style="width: 4em">
                            #{localiser AmountLabel}
                        \
                        <th style="width: 4.5em">
                            #{localiser PriorityLabel}
                        \
                        <th style="width: 8em">
                            #{localiser NotesLabel}
                        \
                    \
                    $forall ItemField itemId name amount priority notes isFinished <- itemsToShow
                        <td .centreAlign>
                            <div .shoppingListCheckbox>
                                <input type="checkbox" ##{pack $ printf "cb%d" itemId} :isFinished:checked>
                                <label for=#{pack $ printf "cb%d" itemId}>
                            \
                            <td .leftAlign>
                                #{name}
                            \
                            <td .centreAlign>
                                #{amount}
                            \
                            <td .centreAlign>
                                #{localiser priority}
                            \
                            <td .leftAlign>
                                #{fromMaybe "" notes}
        |]
