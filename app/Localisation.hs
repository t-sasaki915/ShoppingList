module Localisation
    ( Language (..)
    , Localisable (..)
    , AppTitle (..)
    , ButtonLabel (..)
    , Label (..)
    ) where

import           Data.Text   (Text)
import           Data.Yaml   (FromJSON (..), Value (..))
import           Text.Printf (printf)

data Language = English
              | Japanese
              deriving Show

instance FromJSON Language where
    parseJSON (String "English")  = return English
    parseJSON (String "Japanese") = return Japanese
    parseJSON x                   = fail (printf "Unrecognisable language '%s'." (show x))

class Localisable a where
    localise :: a -> Language -> Text

data AppTitle = AppTitle

instance Localisable AppTitle where
    localise AppTitle English  = "ShoppingList"
    localise AppTitle Japanese = "買い物リスト"

data ButtonLabel = EditButtonLabel
                 | ManageButtonLabel
                 | DeleteButtonLabel
                 | ViewButtonLabel
                 | AddButtonLabel
                 | CancelButtonLabel
                 | DoneButtonLabel

instance Localisable ButtonLabel where
    localise x English = case x of
        EditButtonLabel   -> "Edit"
        ManageButtonLabel -> "Manage"
        DeleteButtonLabel -> "Delete"
        ViewButtonLabel   -> "View"
        AddButtonLabel    -> "Add"
        CancelButtonLabel -> "Cancel"
        DoneButtonLabel   -> "Done"

    localise x Japanese = case x of
        EditButtonLabel   -> "編集"
        ManageButtonLabel -> "管理"
        DeleteButtonLabel -> "削除"
        ViewButtonLabel   -> "閲覧"
        AddButtonLabel    -> "追加"
        CancelButtonLabel -> "中止"
        DoneButtonLabel   -> "完了"

data Label = DoneLabel
           | NameLabel
           | AmountLabel
           | PriorityLabel
           | NotesLabel
           | OperationLabel
           | HideDoneItemsLabel
           | SortOptionLabel

instance Localisable Label where
    localise x English = case x of
        DoneLabel          -> "Done"
        NameLabel          -> "Name"
        AmountLabel        -> "Amount"
        PriorityLabel      -> "Priority"
        NotesLabel         -> "Notes"
        OperationLabel     -> "Operation"
        HideDoneItemsLabel -> "Hide done items"
        SortOptionLabel    -> "Sort: "

    localise x Japanese = case x of
        DoneLabel          -> "完了"
        NameLabel          -> "名称"
        AmountLabel        -> "個数"
        PriorityLabel      -> "重要性"
        NotesLabel         -> "備考"
        OperationLabel     -> "操作"
        HideDoneItemsLabel -> "完了済みを隠す"
        SortOptionLabel    -> "並び替え: "
