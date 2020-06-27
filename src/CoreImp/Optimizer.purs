module CoreImp.Optimizer where

import Prelude
import CoreFn.Ident (Ident(..)) as CF
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..)) as CF
import CoreImp.AST (BinOp(..), Expr(..), Stat(..), UnOp(..), everywhere)
import Data.Array (findIndex, take)
import Data.Foldable (any)
import Data.Maybe (Maybe(..))

optimize :: Stat -> Stat
optimize = enhance <<< necessary
  where
  necessary =
    removeCodeAfterReturnStatements
      <<< removeUndefinedApp

  enhance = inlineCommonOperators

removeCodeAfterReturnStatements :: Stat -> Stat
removeCodeAfterReturnStatements = everywhere convertExpr convertStat
  where
  convertExpr (Function arg stats) = Function arg $ modify stats

  convertExpr a = a

  convertStat (If cond stats) = If cond $ modify stats

  convertStat a = a

  modify ss = case findIndex isReturn ss of
    Just i -> take (i + 1) ss
    Nothing -> ss
    where
    isReturn (Return _) = true

    isReturn _ = false

removeUndefinedApp :: Stat -> Stat
removeUndefinedApp = everywhere convert identity
  where
  convert (Apply f (Variable (CF.Qualified (Just (CF.ModuleName [ (CF.ProperName "Prim") ])) (CF.Ident "undefined")))) = Apply f Unit

  convert a = a

inlineCommonOperators :: Stat -> Stat
inlineCommonOperators = everywhere convert identity
  where
  convert :: Expr -> Expr
  convert a@(Apply (Apply (Apply f d) x) y) = case f, d of
    Variable (CF.Qualified (Just (CF.ModuleName pns)) ident)
    , Variable (CF.Qualified (Just (CF.ModuleName pns')) ident')
      | pns == pns' -> case pns, ident of
        [ (CF.ProperName "Data"), (CF.ProperName "Semiring") ], CF.Ident "add"
          | anyEq [ CF.Ident "semiringInt", CF.Ident "semiringNumber" ] -> Binary Add x y
        [ (CF.ProperName "Data"), (CF.ProperName "Semiring") ], CF.Ident "mul"
          | anyEq [ CF.Ident "semiringInt", CF.Ident "semiringNumber" ] -> Binary Multiply x y
        [ (CF.ProperName "Data"), (CF.ProperName "Ring") ], CF.Ident "sub"
          | anyEq [ CF.Ident "ringInt", CF.Ident "ringNumber" ] -> Binary Subtract x y
        [ (CF.ProperName "Data"), (CF.ProperName "EuclideanRing") ], CF.Ident "div"
          | anyEq [ CF.Ident "euclideanRingInt", CF.Ident "euclideanRingNumber" ] -> Binary Divide x y
        [ (CF.ProperName "Data"), (CF.ProperName "EuclideanRing") ], CF.Ident "mod"
          | anyEq [ CF.Ident "euclideanRingInt", CF.Ident "euclideanRingNumber" ] -> Binary Modulus x y
        [ (CF.ProperName "Data"), (CF.ProperName "Eq") ], CF.Ident "eq"
          | anyEq [ CF.Ident "eqInt", CF.Ident "eqNumber", CF.Ident "eqBoolean", CF.Ident "eqChar", CF.Ident "eqString" ] -> Binary Equal x y
        [ (CF.ProperName "Data"), (CF.ProperName "Eq") ], CF.Ident "notEq"
          | anyEq [ CF.Ident "eqInt", CF.Ident "eqNumber", CF.Ident "eqBoolean", CF.Ident "eqChar", CF.Ident "eqString" ] -> Binary NotEqual x y
        [ (CF.ProperName "Data"), (CF.ProperName "Ord") ], CF.Ident "greaterThan"
          | anyEq [ CF.Ident "ordInt", CF.Ident "ordNumber", CF.Ident "ordBoolean", CF.Ident "ordChar", CF.Ident "ordString" ] -> Binary GreaterThan x y
        [ (CF.ProperName "Data"), (CF.ProperName "Ord") ], CF.Ident "greaterThanEqual"
          | anyEq [ CF.Ident "ordInt", CF.Ident "ordNumber", CF.Ident "ordBoolean", CF.Ident "ordChar", CF.Ident "ordString" ] -> Binary GreaterThanEqual x y
        [ (CF.ProperName "Data"), (CF.ProperName "Ord") ], CF.Ident "lessThan"
          | anyEq [ CF.Ident "ordInt", CF.Ident "ordNumber", CF.Ident "ordBoolean", CF.Ident "ordChar", CF.Ident "ordString" ] -> Binary LessThan x y
        [ (CF.ProperName "Data"), (CF.ProperName "Ord") ], CF.Ident "lessThanEqual"
          | anyEq [ CF.Ident "ordInt", CF.Ident "ordNumber", CF.Ident "ordBoolean", CF.Ident "ordChar", CF.Ident "ordString" ] -> Binary LessThanEqual x y
        [ (CF.ProperName "Data"), (CF.ProperName "HeytingAlgebra") ], CF.Ident "conj"
          | anyEq [ CF.Ident "heytingAlgebraBoolean" ] -> Binary And x y
        [ (CF.ProperName "Data"), (CF.ProperName "HeytingAlgebra") ], CF.Ident "disj"
          | anyEq [ CF.Ident "heytingAlgebraBoolean" ] -> Binary Or x y
        _, _ -> a
        where
        anyEq = any (eq ident')
    _, _ -> a

  convert a@(Apply (Apply f d) x) = case f, d of
    Variable (CF.Qualified (Just (CF.ModuleName pns)) ident)
    , Variable (CF.Qualified (Just (CF.ModuleName pns')) ident')
      | pns == pns' -> case pns, ident of
        [ (CF.ProperName "Data"), (CF.ProperName "Ring") ], CF.Ident "negate"
          | anyEq [ CF.Ident "ringInt", CF.Ident "ringNumber" ] -> Unary Negative x
        [ (CF.ProperName "Data"), (CF.ProperName "HeytingAlgebra") ], CF.Ident "not"
          | anyEq [ CF.Ident "heytingAlgebraBoolean" ] -> Unary Not x
        _, _ -> a
        where
        anyEq = any (eq ident')
    _, _ -> a

  convert a = a
