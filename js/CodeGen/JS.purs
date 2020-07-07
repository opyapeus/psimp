module CodeGen.JS where

import Prelude
import CodeGen.JS.AST as J
import CodeGen.JS.Common (identToJS, properToJS, psstringToJS)
import CoreFn.Ident (Ident) as CF
import CoreFn.Literal (Literal(..)) as CF
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..)) as CF
import CoreImp.AST (BinOp(..), Expr(..), Stat(..), UnOp(..)) as CI
import CoreImp.Module (Module) as CI
import Data.Array (null)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import PSString (PSString(..))

impToJS :: CI.Module -> Array J.Stat
impToJS mod =
  requires
    <> foreignRequires
    <> foreignBinds
    <> map statToJS mod.moduleStats
    <> [ J.Export moduleObj ]
  where
  moduleObj =
    J.Literal <<< J.Object
      $ map
          (\e -> Tuple (identToJS e) (J.Var (identToJS e)))
          mod.moduleExports

  requires =
    map
      (\modName -> J.VarAssign (mkModName modName) (J.Require (joinWith "/" [ "..", mkModPath modName, indexIdent <> ".js" ])))
      mod.moduleImports

  foreignRequires =
    if null mod.moduleForeigns then
      []
    else
      [ J.VarAssign foreignIdent (J.Require (joinWith "/" [ ".", foreignIdent <> ".js" ])) ]

  foreignBinds =
    map (\f -> J.VarAssign (identToJS f) (J.Accessor (identToJS f) (J.Var foreignIdent)))
      mod.moduleForeigns

  qiToExpr :: CF.Qualified CF.Ident -> J.Expr
  qiToExpr (CF.Qualified (Just mn) ident)
    | mn /= mod.moduleName = J.Accessor (identToJS ident) (J.Var (mkModName mn))

  qiToExpr (CF.Qualified _ ident) = J.Var $ identToJS ident

  statToJS :: CI.Stat -> J.Stat
  statToJS (CI.Assign ident expr) = J.VarAssign (identToJS ident) (exprToJS expr)

  statToJS (CI.UpdateAssign new expr) = J.Assign (exprToJS new) (exprToJS expr)

  statToJS (CI.If expr stats) = J.If (exprToJS expr) $ map statToJS stats

  statToJS (CI.Return expr) = J.Return (exprToJS expr)

  statToJS (CI.Throw s) = J.Throw s

  exprToJS :: CI.Expr -> J.Expr
  exprToJS (CI.Literal l) = J.Literal (literal l)

  exprToJS (CI.Accessor k expr) = J.Accessor (psstringToJS k) (exprToJS expr)

  exprToJS (CI.Indexer i expr) = J.Indexer i (exprToJS expr)

  exprToJS (CI.Apply er el) = J.App (exprToJS er) (exprToJS el)

  exprToJS (CI.Variable qi) = qiToExpr qi

  exprToJS (CI.Function arg stats) = J.Function (identToJS arg) $ map statToJS stats

  exprToJS (CI.Binary op x y) = J.Binary (binary op) (exprToJS x) (exprToJS y)

  exprToJS (CI.Unary op x) = J.Unary (unary op) (exprToJS x)

  exprToJS (CI.ObjectClone expr) = J.Clone (exprToJS expr)

  exprToJS (CI.ArrayLength expr) = J.Accessor "length" (exprToJS expr)

  exprToJS CI.Unit = J.Null

  binary :: CI.BinOp -> J.BinOp
  binary CI.Equal = J.Eq

  binary CI.NotEqual = J.Neq

  binary CI.GreaterThan = J.Gt

  binary CI.GreaterThanEqual = J.Gte

  binary CI.LessThan = J.Lt

  binary CI.LessThanEqual = J.Lte

  binary CI.Add = J.Add

  binary CI.Subtract = J.Sub

  binary CI.Multiply = J.Mul

  binary CI.Divide = J.Div

  binary CI.Modulus = J.Mod

  binary CI.And = J.And

  binary CI.Or = J.Or

  unary :: CI.UnOp -> J.UnOp
  unary CI.Negative = J.Neg

  unary CI.Not = J.Not

  literal :: CF.Literal CI.Expr -> J.Lit
  literal (CF.NumericLiteral (Left i)) = J.Int i

  literal (CF.NumericLiteral (Right n)) = J.Number n

  literal (CF.StringLiteral s) = J.String s

  literal (CF.CharLiteral c) = J.String $ fromCharArray [ c ]

  literal (CF.BooleanLiteral b) = J.Boolean b

  literal (CF.ArrayLiteral xs) = J.Array $ map (exprToJS) xs

  literal (CF.ObjectLiteral kvs) = J.Object $ bimap psstringToJS exprToJS <$> lmap PSString <$> kvs

mkModName :: CF.ModuleName -> String
mkModName (CF.ModuleName pns) = joinWith "_" $ map properToJS pns

mkModPath :: CF.ModuleName -> String
mkModPath (CF.ModuleName pns) = joinWith modPathJoiner $ map unProper pns

modPathJoiner :: String
modPathJoiner = "."

foreignIdent :: String
foreignIdent = "foreign"

indexIdent :: String
indexIdent = "index"

unProper :: CF.ProperName -> String
unProper (CF.ProperName x) = x
