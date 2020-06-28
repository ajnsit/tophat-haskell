module Test.Tree where

import Concur.Combinators (list)
import Concur.Core (Widget)
import qualified Concur.Replica as Html
import Concur.Replica.Widgets (Html, inputbox)

data Tree a
  = Tree a (Forest a)

type Forest a = List (Tree a)

init :: Tree Text
init =
  Tree
    "Double click to edit me"
    [ Tree "Or use the 'delete' button to delete me" [],
      Tree "Or use the 'new' button to add a sub node" []
    ]

-- Using Widgets ---------------------------------------------------------------
--
-- Widgets:
-- - Everything you lay out on a page using `Html` tags is *parallel* composition or composition *in space*.
-- - Everything you do inside the monad is *stepwise* composition or composition *in time*.
--
data Action
  = Rename Text
  | Create (Tree Text)
  | Delete
  | Modify (Forest Text)

view :: Maybe (Tree Text) -> Widget Html (Maybe (Tree Text))
view Nothing = Just <|| create
view (Just tree) = view' tree
  where
    view' (Tree title children) = do
      result <-
        Html.ul
          []
          [ Html.li
              []
              [ Rename <|| rename title,
                Create <|| create,
                Delete <|. delete,
                Modify <|| list view' children
              ]
          ]
      pure
        <| case result of
          Rename title' -> Just <| Tree title' children
          Create tree' -> Just <| Tree title (tree' : children)
          Delete -> Nothing
          Modify children' -> Just <| Tree title children'

rename :: Text -> Widget Html Text
rename title = do
  Html.h5 [void Html.onDoubleClick] [Html.text title]
  renamed <- Html.div [] [inputbox title, Html.button [title <|. Html.onClick] [Html.text "Cancel"]]
  pure
    <| if renamed == ""
      then title
      else renamed

create :: Widget Html (Tree Text)
create = do
  Html.button [void Html.onClick] [Html.text "New"]
  pure <| Tree "New Heading" []

delete :: Widget Html ()
delete = do
  Html.button [void Html.onClick] [Html.text "Delete"]
