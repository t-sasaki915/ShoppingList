{-# LANGUAGE ImpredicativeTypes #-}

module WebApp.EditR (getEditR) where

import           Data.Maybe          (fromMaybe)
import           Yesod               (HandlerFor, Html, whamlet)

import qualified Database            as DB
import           Database.WebApp     (handleDB)
import           Item                hiding (itemId)
import           Localisation
import           Localisation.WebApp (localiseHandler)
import           WebApp              (Route (..), WebApp, defaultWebAppLayout)
import           WebApp.ModifyR      (ModifyAction (UpdateItem))

getEditR :: Int -> (HandlerFor WebApp) Html
getEditR itemId = do
    localiser <- localiseHandler

    item <- handleDB $ DB.getItem itemId

    defaultWebAppLayout $
        [whamlet|
            <form method="post" action=@{ModifyR itemId UpdateItem ManageR}>
                <input type="hidden" name="shouldUpdateItemNotes" value="#{True}">

                <div .mainAppHeader>
                    #{localiser AppTitle}
                    <button .button .noVerticalMargin style="float: right" type="submit">#{localiser DoneButtonLabel}
                    <a .button .noVerticalMargin href=@{ManageR} style="float: right">#{localiser CancelButtonLabel}

                <div .shoppingList>
                    <table>
                        <tr>
                            <th>#{localiser NameLabel}
                            <th style="width: 5em">#{localiser AmountLabel}
                            <th style="width: 5em">#{localiser PriorityLabel}
                            <th style="width: 9em">#{localiser NotesLabel}
                        <tr>
                            <td .leftAlign>
                                <input .itemDataInput type="text" name="itemName" required value=#{itemName item}>
                            <td .centreAlign>
                                <input .itemDataInput type="number" name="itemAmount" min="1" required value=#{itemAmount item}>
                            <td .centreAlign>
                                <select .itemDataInput name="itemPriority">
                                    $forall priority <- allItemPriorities
                                        <option value=#{show priority} :priority == itemPriority item:selected>#{localiser priority}
                            <td .leftAlign>
                                <input .itemDataInput type="text" name="itemNotes" value=#{fromMaybe "" (itemNotes item)}>

        |]
