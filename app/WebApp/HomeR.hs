{-# LANGUAGE ImpredicativeTypes #-}

module WebApp.HomeR (getHomeR) where

import           Data.Functor        ((<&>))
import           Data.Maybe          (fromMaybe, isJust, isNothing)
import           Data.Text           (pack)
import           Text.Printf         (printf)
import           Yesod               (HandlerFor, Html, whamlet)

import qualified Database            as DB
import           Database.WebApp     (handleDB)
import           Item                (ItemField (..), ItemOrderOption (..),
                                      allItemOrders, pickAndSortItems)
import           Localisation
import           Localisation.WebApp (localiseHandler)
import           WebApp              (Route (..), WebApp (..),
                                      defaultWebAppLayout)
import           WebApp.ModifyR      (ModifyAction (UpdateItem))

getHomeR :: (HandlerFor WebApp) Html
getHomeR = do
    localiser <- localiseHandler

    orderOpts   <- handleDB DB.getItemOrderOption
    itemsToShow <- handleDB DB.getAllItems <&> pickAndSortItems orderOpts

    defaultWebAppLayout $ do
        [whamlet|
            <div .mainAppHeader>
                #{localiser AppTitle}
                <a .button .noVerticalMargin href=@{ManageR} style="float: right">#{localiser ManageButtonLabel}
        |]

        [whamlet|
            <div .orderOptions>
                <form method="post" action=@{SettingUpdateR}>
                    <div .shoppingListCheckbox .smaller style="float: left; margin: calc((5vw - 4vw) / 2)">
                        <input .submitOnChange name="shouldHideDoneItems" type="checkbox" #hideDoneItems :shouldHideDoneItems orderOpts:checked>
                        <label for="hideDoneItems">
                    <label .centredText for="hideDoneItems">#{localiser HideDoneItemsLabel}
                    <div .spacer>
                    <label for="sortOption" .centredText>#{localiser SortOptionLabel}
                    <select #sortOption name="itemOrder" .selector .submitOnChange>
                        $forall order <- allItemOrders
                            <option :order == itemOrder orderOpts:selected value=#{show order}>#{localiser order}
        |]

        [whamlet|
            <div .shoppingList>
                <table>
                    <tr>
                        <th style="width: 2.5em">#{localiser DoneLabel}
                        <th>#{localiser NameLabel}
                        <th style="width: 4em">#{localiser AmountLabel}
                        <th style="width: 4.5em">#{localiser PriorityLabel}
                        <th style="width: 8em">#{localiser NotesLabel}

                    $forall ItemField itemId name amount priority notes isFinished <- itemsToShow
                        <tr>
                            <td .centreAlign>
                                <div .shoppingListCheckbox>
                                    <form method="post" action=@{ModifyR itemId UpdateItem HomeR}>
                                        <input .submitOnChange type="checkbox" ##{pack $ printf "cb%d" itemId} :isFinished:checked name="itemIsFinished">
                                        <label for=#{pack $ printf "cb%d" itemId}>
                            <td .leftAlign>#{name}
                            <td .centreAlign>#{amount}
                            <td .centreAlign>#{localiser priority}
                            <td :isJust notes:.leftAlign :isNothing notes:.centreAlign>#{fromMaybe "-" notes}
        |]
