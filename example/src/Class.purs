module Class where

class HasColor a where
  color :: a -> String

data Fruit
  = Apple
  | Banana

instance hasColorFruit :: HasColor Fruit where
  color Apple = "red"
  color Banana = "yellow"

newtype Prize
  = Prize Int

instance hasColorPrize :: HasColor Prize where
  color (Prize 1) = "gold"
  color (Prize 2) = "silver"
  color (Prize 3) = "brons"
  color (Prize _) = "white"

appleColor :: String
appleColor = color Apple
