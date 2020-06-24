module Sample where

import Prelude

eo :: Int -> String
eo x =
  if mod x 2 == 0 then
    "even"
  else
    "odd"
