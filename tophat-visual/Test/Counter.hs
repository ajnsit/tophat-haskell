module Test.Counter where

import Concur.Core hiding (display)
import Concur.Replica
import Prelude hiding (div)

counter :: Int -> Widget HTML a
counter x = do
  click <-
    div
      []
      [ Left <|| button [onClick] [text "-"],
        text <| display x,
        Right <|| button [onClick] [text "+"]
      ]
  case click of
    Left _ -> counter (x - 1)
    Right _ -> counter (x + 1)
