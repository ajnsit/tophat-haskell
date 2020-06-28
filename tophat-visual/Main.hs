module Main where

-- import Concur.Combinators (loop)
import Concur.Replica.Run (runDefault)
-- import qualified Test.Temperature as Widgets
import qualified Test.Wire as Widgets

main :: IO ()
-- main = runDefault 8080 "Replica App" (loop Widgets.temperature (37.0, 37.0))
main = runDefault 8080 "Replica App" (const (Widgets.temperature 37.0))
