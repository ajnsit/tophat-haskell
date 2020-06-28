module Test.Form where

import Concur.Combinators (list')
import Concur.Core (Widget)
import qualified Concur.Replica as Html
import Concur.Replica.Widgets (Html)

data Form = Form
  { _name :: Text,
    _rememberMe :: Bool
  }

init :: Form
init = Form {_name = "Tim", _rememberMe = False}

data Action
  = Name Text
  | RememberMe Bool
  | Submit

render :: Form -> Widget Html Form
render form = do
  res <-
    Html.div
      []
      [ Name
          <|| Html.input
            [ Html.type_ "text",
              Html.value <| _name form,
              Html.targetValue <|| Html.target <|| Html.onChange
            ],
        RememberMe (not <| _rememberMe form)
          <|. Html.input
            [ Html.type_ "checkbox",
              Html.checked <| _rememberMe form,
              Html.onChange
            ],
        Submit
          <|. Html.button [Html.onClick] [Html.text "Submit"]
      ]
  case res of
    Name s -> render (form {_name = s})
    RememberMe b -> render (form {_rememberMe = b})
    Submit -> pure form

stepByStep :: List Form -> Widget Html (List Form)
stepByStep = traverse render

sideBySide :: List Form -> Widget Html (List Form)
sideBySide = list' render
