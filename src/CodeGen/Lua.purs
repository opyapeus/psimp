module CodeGen.Lua where

import Prelude
import CodeGen.Lua.AST (Expr(..), Stat(..))
import CodeGen.Lua.AST as L
import CodeGen.Lua.Common (identToLua, properToLua, psstringToLua)
import CoreFn.Ident (Ident) as CF
import CoreFn.Literal (Literal(..)) as CF
import CoreFn.Names (ModuleName(..), Qualified(..)) as CF
import CoreImp.AST (BinOp(..), Expr(..), Stat(..), UnOp(..)) as CI
import CoreImp.Module (Module) as CI
import Data.Array (cons, elem, foldr, null, uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import PSString (PSString(..))

impToLua :: forall a. CI.Module a -> Array L.Stat
impToLua mod =
  imps
    <> fimp
    <> fbind
    <> map statToLua mod.moduleStats
    <> [ L.Return modObj ]
  where
  modObj =
    L.Literal <<< L.Object
      $ map
          (\e -> Tuple (identToLua e) (L.Var (identToLua e)))
          mod.moduleExports

  imps =
    map
      (\modName -> L.LocalAssign (mkModName modName) (L.Require (joinWith "/" [ mkModName modName, indexIdent ])))
      mod.moduleImports

  fimp =
    if null mod.moduleForeigns then
      []
    else
      [ L.LocalAssign foreignIdent (L.Require (joinWith "/" [ mkModName mod.moduleName, foreignIdent ])) ]

  fbind =
    map (\f -> LocalAssign (identToLua f) (L.Accessor (identToLua f) (L.Var foreignIdent)))
      mod.moduleForeigns

  qiToExpr :: CF.Qualified CF.Ident -> Expr
  qiToExpr (CF.Qualified (Just mn) ident)
    | mn /= mod.moduleName = Accessor (identToLua ident) (L.Var (mkModName mn))
    | mn == mod.moduleName, elem ident mod.moduleForeigns = Accessor (identToLua ident) (L.Var foreignIdent)

  qiToExpr (CF.Qualified _ ident) = L.Var $ identToLua ident

  statToLua :: CI.Stat a -> L.Stat
  statToLua (CI.Assign ident expr) = L.LocalAssign (identToLua ident) (exprToLua expr)

  statToLua (CI.If expr stats) = L.If (exprToLua expr) $ map statToLua stats

  statToLua (CI.Return expr) = L.Return (exprToLua expr)

  exprToLua :: CI.Expr a -> L.Expr
  exprToLua (CI.Literal l) = L.Literal (literal l)

  exprToLua (CI.Constructor cn idents) = go idents []
    where
    go :: Array CF.Ident -> Array L.Stat -> L.Expr
    go [] done =
      letIn
        $ [ L.LocalAssign cn'
              ( L.Literal
                  ( L.Object
                      [ Tuple ctorTagIdent (L.Literal (L.String cn')) ]
                  )
              )
          ]
        <> done
        <> [ L.Return obj ]

    go is done = case uncons is of
      Just { head: i, tail: rem } -> L.Function arg [ L.Return (go rem done') ]
        where
        arg = identToLua i

        done' = done <> [ L.Assign (L.Accessor arg obj) (L.Var arg) ]
      Nothing -> L.Nil -- unreachable      

    cn' = properToLua cn

    obj = L.Var cn'

  exprToLua (CI.Accessor k expr) = L.Accessor (psstringToLua k) (exprToLua expr)

  exprToLua (CI.Indexer i expr) = L.Indexer (luaIndex i) (exprToLua expr)

  exprToLua (CI.ObjectUpdate expr kvs) =
    letIn
      $ map
          (\(Tuple k v) -> L.Assign (L.Accessor (psstringToLua k) (exprToLua expr)) (exprToLua v))
          kvs
      <> [ L.Return (exprToLua expr) ]

  exprToLua (CI.Apply er el) = L.App (exprToLua er) (exprToLua el)

  exprToLua (CI.Variable qi) = qiToExpr qi

  exprToLua (CI.Function arg stats) = L.Function (identToLua arg) $ map statToLua stats

  exprToLua (CI.LetIn stats) = letIn $ map statToLua stats

  exprToLua (CI.TagOf cn expr) =
    L.Binary
      L.Eq
      (L.Accessor ctorTagIdent (exprToLua expr))
      (L.Literal (L.String (properToLua (unQualified cn))))

  exprToLua (CI.Binary op x y) =
    L.Binary
      (binary op)
      (exprToLua x)
      (exprToLua y)

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

  literal :: CF.Literal (CI.Expr a) -> L.Lit
  literal (CF.NumericLiteral (Left i)) = L.Int i

  literal (CF.NumericLiteral (Right n)) = L.Number n

  literal (CF.StringLiteral s) = L.String s

  literal (CF.CharLiteral c) = L.String $ fromCharArray [ c ]

  literal (CF.BooleanLiteral b) = L.Boolean b

  literal (CF.ArrayLiteral xs) = L.Array $ map (exprToLua) xs

  literal (CF.ObjectLiteral kvs) =
    L.Object
      $ foldr
          (\(Tuple k v) -> cons (Tuple (psstringToLua k) (exprToLua v)))
          []
          (lmap PSString <$> kvs)

letIn :: Array Stat -> Expr
letIn stats = L.App (L.Function "" stats) L.Nil

mkModName :: CF.ModuleName -> String
mkModName (CF.ModuleName pns) = joinWith "_" $ map properToLua pns

ctorTagIdent :: String
ctorTagIdent = "tag"

foreignIdent :: String
foreignIdent = "foreign"

indexIdent :: String
indexIdent = "index"

-- NOTE: lua index starts with 1
luaIndex :: Int -> Int
luaIndex = (+) 1

unQualified :: forall a. CF.Qualified a -> a
unQualified (CF.Qualified _ x) = x
