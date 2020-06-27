module CodeGen.Lua where

import Prelude
import CodeGen.Lua.AST as L
import CodeGen.Lua.Common (identToLua, properToLua, psstringToLua)
import CoreFn.Ident (Ident) as CF
import CoreFn.Literal (Literal(..)) as CF
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..)) as CF
import CoreImp.AST (BinOp(..), Expr(..), Stat(..), UnOp(..)) as CI
import CoreImp.Module (Module) as CI
import Data.Array (elem, null)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import PSString (PSString(..))

impToLua :: CI.Module -> Array L.Stat
impToLua mod =
  requires
    <> foreignRequires
    <> foreignBinds
    <> map statToLua mod.moduleStats
    <> [ L.Return moduleObj ]
  where
  moduleObj =
    L.Literal <<< L.Object
      $ map
          (\e -> Tuple (identToLua e) (L.Var (identToLua e)))
          mod.moduleExports

  requires =
    map
      (\modName -> L.LocalAssign (mkModName modName) (L.Require (joinWith "/" [ mkModPath modName, indexIdent ])))
      mod.moduleImports

  foreignRequires =
    if null mod.moduleForeigns then
      []
    else
      [ L.LocalAssign foreignIdent (L.Require (joinWith "/" [ mkModPath mod.moduleName, foreignIdent ])) ]

  foreignBinds =
    map (\f -> L.LocalAssign (identToLua f) (L.Accessor (identToLua f) (L.Var foreignIdent)))
      mod.moduleForeigns

  qiToExpr :: CF.Qualified CF.Ident -> L.Expr
  qiToExpr (CF.Qualified (Just mn) ident)
    | mn /= mod.moduleName = L.Accessor (identToLua ident) (L.Var (mkModName mn))
    | mn == mod.moduleName, elem ident mod.moduleForeigns = L.Accessor (identToLua ident) (L.Var foreignIdent)

  qiToExpr (CF.Qualified _ ident) = L.Var $ identToLua ident

  statToLua :: CI.Stat -> L.Stat
  statToLua (CI.Assign ident expr) = L.LocalAssign (identToLua ident) (exprToLua expr)

  statToLua (CI.UpdateAssign new expr) = L.Assign (exprToLua new) (exprToLua expr)

  statToLua (CI.ObjectCopy ident expr) = L.LocalAssign (identToLua ident) (L.Clone (exprToLua expr))

  statToLua (CI.If expr stats) = L.If (exprToLua expr) $ map statToLua stats

  statToLua (CI.Return expr) = L.Return (exprToLua expr)

  statToLua (CI.Throw s) = L.Throw s

  exprToLua :: CI.Expr -> L.Expr
  exprToLua (CI.Literal l) = L.Literal (literal l)

  exprToLua (CI.Accessor k expr) = L.Accessor (psstringToLua k) (exprToLua expr)

  exprToLua (CI.Indexer i expr) = L.Indexer (luaIndex i) (exprToLua expr)

  exprToLua (CI.Apply er el) = L.App (exprToLua er) (exprToLua el)

  exprToLua (CI.Variable qi) = qiToExpr qi

  exprToLua (CI.Function arg stats) = L.Function (identToLua arg) $ map statToLua stats

  exprToLua (CI.Binary op x y) = L.Binary (binary op) (exprToLua x) (exprToLua y)

  exprToLua (CI.Unary op x) = L.Unary (unary op) (exprToLua x)

  exprToLua CI.Unit = L.Nil

  binary :: CI.BinOp -> L.BinOp
  binary CI.Equal = L.Eq

  binary CI.NotEqual = L.Neq

  binary CI.GreaterThan = L.Gt

  binary CI.GreaterThanEqual = L.Gte

  binary CI.LessThan = L.Lt

  binary CI.LessThanEqual = L.Lte

  binary CI.Add = L.Add

  binary CI.Subtract = L.Sub

  binary CI.Multiply = L.Mul

  binary CI.Divide = L.Div

  binary CI.Modulus = L.Mod

  binary CI.And = L.And

  binary CI.Or = L.Or

  unary :: CI.UnOp -> L.UnOp
  unary CI.Negative = L.Neg

  unary CI.Length = L.Len

  unary CI.Not = L.Not

  literal :: CF.Literal CI.Expr -> L.Lit
  literal (CF.NumericLiteral (Left i)) = L.Int i

  literal (CF.NumericLiteral (Right n)) = L.Number n

  literal (CF.StringLiteral s) = L.String s

  literal (CF.CharLiteral c) = L.String $ fromCharArray [ c ]

  literal (CF.BooleanLiteral b) = L.Boolean b

  literal (CF.ArrayLiteral xs) = L.Array $ map (exprToLua) xs

  literal (CF.ObjectLiteral kvs) = L.Object $ bimap psstringToLua exprToLua <$> lmap PSString <$> kvs

mkModName :: CF.ModuleName -> String
mkModName (CF.ModuleName pns) = joinWith "_" $ map properToLua pns

mkModPath :: CF.ModuleName -> String
mkModPath (CF.ModuleName pns) = joinWith modPathJoiner $ map unProper pns

modPathJoiner :: String
modPathJoiner = "_"

foreignIdent :: String
foreignIdent = "foreign"

indexIdent :: String
indexIdent = "index"

-- NOTE: lua index starts with 1
luaIndex :: Int -> Int
luaIndex = (+) 1

unProper :: CF.ProperName -> String
unProper (CF.ProperName x) = x
