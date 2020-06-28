module Test.Steps where

import Concur.Core (Widget)
import qualified Concur.Replica as Html
import Concur.Replica.Widgets (Html, button, inputbox)

hitter :: Widget Html Text
hitter = do
  _ <- Html.button [Html.onClick] [Html.text "Hit me!"]
  _ <- Html.button [Html.onClick] [Html.text "Hit me again!"]
  Html.text "I'm brused"

putter :: Widget Html Text
putter = do
  value <- Html.input [Html.type_ "text", Html.targetValue <|| Html.target <|| Html.onChange]
  _ <- Html.button [Html.onClick] [Html.text value]
  Html.text "I'm filled"

boxer :: Widget Html Text
boxer = do
  label <- inputbox "Default"
  _ <- button label
  Html.text "I'm done"
