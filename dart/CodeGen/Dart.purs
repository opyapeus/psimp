module CodeGen.Dart where

import Prelude
import CodeGen.Dart.AST as D
import CodeGen.Dart.Common (identToDart, private, properToDart, psstringToDart, public)
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
import PSString (PSString(..))

impToDart :: CI.Module -> Array D.Stat
impToDart mod =
  imps
    <> fimps
    <> fbinds
    <> map statToGoTL mod.moduleStats
    <> exps
  where
  imps =
    map
      (\modName -> D.Import (joinWith "/" [ "..", mkModPath modName, indexIdent <> ".dart" ]) (Just (mkModName modName)))
      mod.moduleImports

  fimps
    | null mod.moduleForeigns = []
    | otherwise = [ D.Import (joinWith "/" [ ".", foreignIdent <> ".dart" ]) (Just foreignIdent) ]

  fbinds =
    map (\f -> D.VarAssign (private f) (D.Referrer foreignIdent (public f)))
      mod.moduleForeigns

  exps =
    map (\e -> D.VarAssign (public e) (D.Var (private e)))
      mod.moduleExports

  qiToExpr :: CF.Qualified CF.Ident -> D.Expr
  qiToExpr (CF.Qualified (Just mn) ident)
    | mn /= mod.moduleName = D.Referrer (mkModName mn) (public ident)
    | otherwise = D.Var $ private ident

  qiToExpr (CF.Qualified _ ident) = D.Var $ identToDart ident

  -- NOTE: this is for separating private and public var names.
  statToGoTL :: CI.Stat -> D.Stat
  statToGoTL (CI.Assign ident expr) = D.VarAssign (private ident) (exprToDart expr)

  statToGoTL _ = D.Throw "Not assign statement in top level."

  statToDart :: CI.Stat -> D.Stat
  statToDart (CI.Assign ident expr) = D.VarAssign (identToDart ident) (exprToDart expr)

  statToDart (CI.UpdateAssign new expr) = D.Assign (exprToDart new) (exprToDart expr)

  statToDart (CI.ObjectCopy ident expr) = D.VarAssign (identToDart ident) (D.Clone (exprToDart expr))

  statToDart (CI.If expr stats) = D.If (exprToDart expr) $ map statToDart stats

  statToDart (CI.Return expr) = D.Return (exprToDart expr)

  statToDart (CI.Throw s) = D.Throw s

  exprToDart :: CI.Expr -> D.Expr
  exprToDart (CI.Literal l) = D.Literal (literal l)

  exprToDart (CI.Accessor k expr) = D.Accessor (psstringToDart k) (exprToDart expr)

  exprToDart (CI.Indexer i expr) = D.Indexer i (exprToDart expr)

  exprToDart (CI.Apply er el) = D.App (exprToDart er) (exprToDart el)

  exprToDart (CI.Variable qi) = qiToExpr qi

  exprToDart (CI.Function arg stats) = D.Function (identToDart arg) $ map statToDart stats

  exprToDart (CI.Binary op x y) = D.Binary (binary op) (exprToDart x) (exprToDart y)

  exprToDart (CI.Unary op x) = case op of
    CI.Negative -> D.Unary D.Neg (exprToDart x)
    CI.Not -> D.Unary D.Not (exprToDart x)
    CI.Length -> D.Length (exprToDart x)

  exprToDart CI.Unit = D.Null

  binary :: CI.BinOp -> D.BinOp
  binary CI.Equal = D.Eq

  binary CI.NotEqual = D.Neq

  binary CI.GreaterThan = D.Gt

  binary CI.GreaterThanEqual = D.Gte

  binary CI.LessThan = D.Lt

  binary CI.LessThanEqual = D.Lte

  binary CI.Add = D.Add

  binary CI.Subtract = D.Sub

  binary CI.Multiply = D.Mul

  binary CI.Divide = D.Div

  binary CI.Modulus = D.Mod

  binary CI.And = D.And

  binary CI.Or = D.Or

  literal :: CF.Literal CI.Expr -> D.Lit
  literal (CF.NumericLiteral (Left i)) = D.Int i

  literal (CF.NumericLiteral (Right n)) = D.Number n

  literal (CF.StringLiteral s) = D.String s

  literal (CF.CharLiteral c) = D.String $ fromCharArray [ c ]

  literal (CF.BooleanLiteral b) = D.Boolean b

  literal (CF.ArrayLiteral xs) = D.Array $ map (exprToDart) xs

  literal (CF.ObjectLiteral kvs) = D.Object $ bimap psstringToDart exprToDart <$> lmap PSString <$> kvs

mkModName :: CF.ModuleName -> String
mkModName (CF.ModuleName pns) = joinWith "_" $ map properToDart pns

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
