module CoreImp.Misc where

import Prelude
import CoreFn.Literal as CF
import Data.Traversable (traverse)

-- NOTE: this should be in CoreFn?
traverseLiteral ::
  forall f a b.
  Functor f =>
  Applicative f =>
  (a -> f b) -> CF.Literal a -> f (CF.Literal b)
traverseLiteral f (CF.ArrayLiteral xs) = CF.ArrayLiteral <$> traverse f xs

traverseLiteral f (CF.ObjectLiteral xs) = CF.ObjectLiteral <$> traverse (traverse f) xs

traverseLiteral _ (CF.NumericLiteral x) = CF.NumericLiteral <$> pure x

traverseLiteral _ (CF.StringLiteral x) = CF.StringLiteral <$> pure x

traverseLiteral _ (CF.CharLiteral x) = CF.CharLiteral <$> pure x

traverseLiteral _ (CF.BooleanLiteral x) = CF.BooleanLiteral <$> pure x
