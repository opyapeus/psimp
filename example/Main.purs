module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Sample (eo, fact)

main :: Effect Unit
main = do
  let
    odd = eo 1

    even = eo 2
  log odd
  log even
  log <<< show $ fact 7
