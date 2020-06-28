module Test.Temperature where

import Concur.Core hiding (display)
import qualified Concur.Replica as Html
import Concur.Replica.Widgets (Html, floatValue)

temperature :: (Double, Double) -> Widget Html (Double, Double)
temperature (c, f) = do
  new <-
    Html.div
      []
      [ Html.text "Celsius:",
        Html.input [Html.value <| display c, map Left <|| floatValue <|| Html.onInput],
        Html.text "Fahrenheit:",
        Html.input [Html.value <| display f, map Right <|| floatValue <|| Html.onInput]
      ]
  case spy "New value" new of
    Nothing -> temperature (c, f)
    Just (Left c') -> pure (c', c2f c')
    Just (Right f') -> pure (f2c f', f')

c2f :: Double -> Double
c2f c = (c * 9.0 / 5.0) + 32.0

f2c :: Double -> Double
f2c f = ((f - 32.0) * 5.0) / 9.0
