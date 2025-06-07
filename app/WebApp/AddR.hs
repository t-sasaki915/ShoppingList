{-# LANGUAGE ImpredicativeTypes #-}

module WebApp.AddR (getAddR, postAddR) where

import           Data.Text           (Text)
import           Data.Text.TRead     (tRead)
import           Yesod

import qualified Database            as DB
import           Database.WebApp     (handleDB)
import           Item                (allItemPriorities, defaultItemPriority)
import           Localisation
import           Localisation.WebApp (localiseHandler)
import           WebApp              (Route (..), WebApp (..),
                                      defaultWebAppLayout)

getAddR :: (HandlerFor WebApp) Html
getAddR = do
    localiser <- localiseHandler

    defaultWebAppLayout $
        [whamlet|
            <form method="post" action=@{AddR}>
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
                                <input .itemDataInput type="text" value="" name="itemName" required>
                            <td .centreAlign>
                                <input .itemDataInput type="number" value="1" min="1" name="itemAmount">
                            <td .centreAlign>
                                <select .itemDataInput name="itemPriority">
                                    $forall itemPriority <- allItemPriorities
                                        <option :itemPriority == defaultItemPriority:selected value=#{show itemPriority}>#{localiser itemPriority}
                            <td .leftAlign>
                                <input .itemDataInput type="text" value="" name="itemNotes">
        |]

data ItemAddRequest = ItemAddRequest
    { itemName     :: Text
    , itemAmount   :: Int
    , itemPriority :: Text
    , itemNotes    :: Maybe Text
    }

postAddR :: (HandlerFor WebApp) ()
postAddR = do
    req <- runInputPost $ ItemAddRequest
        <$> ireq textField "itemName"
        <*> ireq intField  "itemAmount"
        <*> ireq textField "itemPriority"
        <*> iopt textField "itemNotes"

    handleDB $ DB.addItem (itemName req) (itemAmount req) (tRead $ itemPriority req) (itemNotes req)

    redirect ManageR
