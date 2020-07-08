module ObjectUpdate where

import Prelude

type Person
  = { name :: String
    , age :: Int
    }

passYearPerson :: Person -> Person
passYearPerson p = p { age = p.age + 1 }

-- ObjectUpdate
passYear :: forall a. { age :: Int | a } -> { age :: Int | a }
passYear x = x { age = x.age + 1 }
