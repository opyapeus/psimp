-- NOTE: this should be in CoreFn
module PSString where

import Prelude

newtype PSString
  = PSString String

derive instance eqPSString :: Eq PSString

derive instance ordPSString :: Ord PSString

instance showPSString :: Show PSString where
  show (PSString s) = "(PSString " <> show s <> ")"
