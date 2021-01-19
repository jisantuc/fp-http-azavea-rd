module Main where

import Data.Show (show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console as Console
import Model (extent)

main :: Effect Unit
main = Console.log (show extent)