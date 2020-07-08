module EmptyAnn where

import Prelude
import CoreFn.Ann (Ann)
import CoreFn.Annotation (class Annotation, annFromJSON)

data EmptyAnn
  = EmptyAnn

derive instance eqEmptyAnn :: Eq EmptyAnn

instance showEmptyAnn :: Show EmptyAnn where
  show EmptyAnn = "EmptyAnn"

instance annotationEmptyAnn :: Annotation EmptyAnn where
  annFromJSON modulePath = map annToVoid <<< annFromJSON modulePath
    where
    annToVoid :: Ann -> EmptyAnn
    annToVoid _ = EmptyAnn
