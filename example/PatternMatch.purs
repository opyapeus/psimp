module PatternMatch where

import Prelude

-- Constructor
data Something
  = Sum
  | Product Int String

cpm :: Something -> String
cpm Sum = "sum"

cpm (Product i s)
  | i == 7 = "product seven"
  | s == "lucky" = "product lucky"

cpm _ = "some product"

type Person
  = { name :: String
    , age :: Int
    }

-- Object
opm :: Person -> String
opm { name: _, age: 20 } = "twenty"

opm { name: "john", age: 40 } = "someone"

opm { name, age } = name <> show age

-- Array
apm :: Array String -> String
apm [] = "empty"

apm [ a ] = a

apm [ a, b ] = a <> "," <> b

apm _ = "some"

-- Newtype
newtype Name
  = Name String

npm :: Name -> String
npm (Name s) = s
