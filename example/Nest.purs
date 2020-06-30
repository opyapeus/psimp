module Nest where

import Prelude

-- ObjectUpdate
upd ::
  forall tx ty.
  { a0 :: { a1 :: Int | ty }, b0 :: Int | tx } ->
  { a0 :: { a1 :: Int | ty }, b0 :: Int | tx }
upd o =
  o
    { a0
      { a1 = o.a0.a1 + 1
      }
    , b0 = o.b0 - 1
    }

arrLit :: Array (Array Int)
arrLit =
  [ [ 1, 2, 3 ]
  , [ 4, 5, 6 ]
  ]

objLit :: { a :: { b :: Int } }
objLit =
  { a:
      { b: 1
      }
  }
