module Sample where

import Prelude

f :: Int -> String
f x =
  if mod x 2 == 0 then
    "even"
  else
    "odd"
