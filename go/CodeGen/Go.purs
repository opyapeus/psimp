module CodeGen.Go where

import Prelude
import CodeGen.Go.AST as G
import CodeGen.Go.Common (private, properToGo, psstringToGo, public)
import CoreFn.Ident (Ident) as CF
import CoreFn.Literal (Literal(..)) as CF
import CoreFn.Names (ModuleName(..), Qualified(..)) as CF
import CoreImp.AST (BinOp(..), Expr(..), Stat(..), UnOp(..)) as CI
import CoreImp.Module (Module) as CI
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import PSString (PSString(..))

impToGo :: CI.Module -> Array G.Stat
impToGo mod =
  pkg
    <> imp
    <> [ G.Raw "type _ = Any" ] -- NOTE: to prevent error "imported and not used"
    <> map statToGo mod.moduleStats
    <> exps
  where
  pkg = [ G.Package (mkModName mod.moduleName) ]

  imp = [ G.Import imps ]
    where
    imps =
      [ Tuple (Just G.Dot) (joinWith "/" [ "..", "_Helper" ]) ]
        <> map
            (\modName -> Tuple Nothing (joinWith "/" [ "..", mkModName modName ]))
            mod.moduleImports

  exps =
    map (\e -> G.VarAssign (public e) (G.Var (private e)))
      mod.moduleExports

  qiToExpr :: CF.Qualified CF.Ident -> G.Expr
  qiToExpr (CF.Qualified (Just mn) ident)
    | mn /= mod.moduleName = G.Reference (mkModName mn) (G.Var (public ident))

  qiToExpr (CF.Qualified _ ident) = G.Var (private ident)

  statToGo :: CI.Stat -> G.Stat
  statToGo (CI.Assign ident expr) = G.VarAssign (private ident) (exprToGo expr)

  statToGo (CI.UpdateAssign new expr) = G.Assign (exprToGo new) (exprToGo expr)

  statToGo (CI.ObjectCopy ident expr) = G.VarAssign (private ident) (G.Clone (exprToGo expr))

  statToGo (CI.If expr stats) = G.If (exprToGo expr) $ map statToGo stats

  statToGo (CI.Return expr) = G.Return (exprToGo expr)

  statToGo (CI.Throw s) = G.Throw s

  exprToGo :: CI.Expr -> G.Expr
  exprToGo (CI.Literal l) = G.Literal (literal l)

  exprToGo (CI.Accessor k expr) = G.Accessor (psstringToGo k) (exprToGo expr)

  exprToGo (CI.Indexer i expr) = G.Indexer i (exprToGo expr)

  exprToGo (CI.Apply er el) = G.App (exprToGo er) (exprToGo el)

  exprToGo (CI.Variable qi) = qiToExpr qi

  exprToGo (CI.Function arg stats) = G.Function (private arg) $ map statToGo stats

  exprToGo (CI.Binary op x y) = G.Binary (binary op) (exprToGo x) (exprToGo y)

  exprToGo (CI.Unary op x) = G.Unary (unary op) (exprToGo x)

  exprToGo CI.Unit = G.Nil

  binary :: CI.BinOp -> G.BinOp
  binary CI.Equal = G.Eq

  binary CI.NotEqual = G.Neq

  binary CI.GreaterThan = G.Gt

  binary CI.GreaterThanEqual = G.Gte

  binary CI.LessThan = G.Lt

  binary CI.LessThanEqual = G.Lte

  binary CI.Add = G.Add

  binary CI.Subtract = G.Sub

  binary CI.Multiply = G.Mul

  binary CI.Divide = G.Div

  binary CI.Modulus = G.Mod

  binary CI.And = G.And

  binary CI.Or = G.Or

  unary :: CI.UnOp -> G.UnOp
  unary CI.Negative = G.Neg

  unary CI.Length = G.Len

  unary CI.Not = G.Not

  literal :: CF.Literal CI.Expr -> G.Lit
  literal (CF.NumericLiteral (Left i)) = G.Int i

  literal (CF.NumericLiteral (Right n)) = G.Number n

  literal (CF.StringLiteral s) = G.String s

  literal (CF.CharLiteral c) = G.String $ fromCharArray [ c ]

  literal (CF.BooleanLiteral b) = G.Boolean b

  literal (CF.ArrayLiteral xs) = G.Array $ map (exprToGo) xs

  literal (CF.ObjectLiteral kvs) = G.Object $ bimap psstringToGo exprToGo <$> lmap PSString <$> kvs

mkModName :: CF.ModuleName -> String
mkModName (CF.ModuleName pns) = joinWith "_" $ map properToGo pns

ctorMetaIdent :: String
ctorMetaIdent = "tag"

foreignIdent :: String
foreignIdent = "foreign"

indexIdent :: String
indexIdent = "index"

unQualified :: forall a. CF.Qualified a -> a
unQualified (CF.Qualified _ x) = x
