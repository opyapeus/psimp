module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Sample (f)

main :: Effect Unit
main = do
  let
    r = f 1
  log r
