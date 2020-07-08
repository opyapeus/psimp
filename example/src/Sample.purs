module Sample where

import Prelude

eo :: Int -> String
eo x =
  if mod x 2 == 0 then
    "even"
  else
    "odd"

fact :: Int -> Int
fact 0 = 1

fact n
  | n < 0 = fact (-n)
  | otherwise = n * fact (n - 1)
