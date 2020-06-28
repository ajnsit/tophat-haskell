module Test.Wire where

import Concur.Core hiding (display)
import qualified Concur.Replica as Html
import Concur.Replica.Widgets (Html, floatValue)
import Concur.Wire
import qualified Lens.Simple as Lens

-- Example ---------------------------------------------------------------------
counter :: Wire Int -> Widget Html Unit
counter w = do
  let x = value w
  void <| Html.button [Html.onClick] [Html.text <| display x]
  if x < 10
    then send w <| x + 1
    else pure ()

counterWithMessage :: Wire Int -> Widget Html a
counterWithMessage w = do
  counter w
  Html.div [] [Html.text "Counter finished"]

counters1 :: Widget Html a
counters1 = with 0 \w -> Html.div [] [counterWithMessage w, counterWithMessage w]

counters2 :: Widget Html a
counters2 =
  with (0, 0) \wire ->
    Html.div
      []
      [ Html.div [] [Html.text "These two counters have different states"],
        counterWithMessage (focus Lens._1 wire),
        counterWithMessage (focus Lens._2 wire),
        Html.div [] [Html.text "This counter is equal to the first one "],
        counterWithMessage (focus Lens._1 wire)
      ]

temperature :: Double -> Widget Html void
temperature start = with start view
  where
    view wire = do
      let c = wire
          f = focus (Lens.iso c2f f2c) wire
      new <-
        Html.div
          []
          [ Html.text "Celsius:",
            Html.input [Html.value (display <| value c), map Left <|| floatValue <|| Html.onInput],
            Html.text "Fahrenheit:",
            Html.input [Html.value (display <| value f), map Right <|| floatValue <|| Html.onInput]
          ]
      case spy "New value" new of
        Nothing -> pure ()
        Just (Left c') -> send c c'
        Just (Right f') -> send f f'
      view wire

c2f :: Double -> Double
c2f c = (c * 9.0 / 5.0) + 32.0

f2c :: Double -> Double
f2c f = ((f - 32.0) * 5.0) / 9.0
