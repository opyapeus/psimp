module Sample where

import Prelude

eo :: Int -> String
eo x =
  if mod x 2 == 0 then
    "even"
  else
    "odd"

frac :: Int -> Int
frac 0 = 1

frac n
  | n < 0 = frac (-n)
  | otherwise = n * frac (n - 1)
