{-# LANGUAGE ImpredicativeTypes #-}

module WebApp.ManageR (getManageR) where

import           Data.Maybe          (fromMaybe, isJust, isNothing)
import           Yesod               (HandlerFor, Html, whamlet)

import qualified Database            as DB
import           Database.WebApp     (handleDB)
import           Item                (ItemField (..))
import           Localisation
import           Localisation.WebApp (localiseHandler)
import           WebApp              (Route (..), WebApp (..),
                                      defaultWebAppLayout)
import           WebApp.ModifyR      (ModifyAction (DeleteItem))

getManageR :: (HandlerFor WebApp) Html
getManageR = do
    localiser <- localiseHandler

    allItems <- handleDB DB.getAllItems

    defaultWebAppLayout $ do
        [whamlet|
            <div .mainAppHeader>
                #{localiser AppTitle}
                <a .button .noVerticalMargin href=@{HomeR} style="float: right">#{localiser ViewButtonLabel}
                <a .button .noVerticalMargin href=@{AddR} style="float: right">#{localiser AddButtonLabel}
        |]

        [whamlet|
            <div .shoppingList>
                <table>
                    <tr>
                        <th>#{localiser NameLabel}
                        <th style="width: 4em">#{localiser AmountLabel}
                        <th style="width: 4.5em">#{localiser PriorityLabel}
                        <th style="width: 7em">#{localiser NotesLabel}
                        <th style="width: 5em">#{localiser OperationLabel}

                    $forall ItemField itemId name amount priority notes _ <- allItems
                        <tr>
                            <td .leftAlign>#{name}
                            <td .centreAlign>#{amount}
                            <td .centreAlign>#{localiser priority}
                            <td :isJust notes:.leftAlign :isNothing notes:.centreAlign>#{fromMaybe "-" notes}
                            <td .centreAlign>
                                <a .button .noHorizontalMargin href=@{EditR itemId}>#{localiser EditButtonLabel}
                                <form method="post" action=@{ModifyR itemId DeleteItem ManageR}>
                                    <button .button .noHorizontalMargin type="submit">#{localiser DeleteButtonLabel}
        |]
