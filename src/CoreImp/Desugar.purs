module CoreImp.Desugar where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import CoreFn.Binders (Binder(..)) as CF
import CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..), Guard) as CF
import CoreFn.Ident (Ident(..)) as CF
import CoreFn.Literal (Literal(..)) as CF
import CoreFn.Module (Module(..), ModuleImport(..)) as CF
import CoreImp.AST (BinOp(..), Expr(..), Stat(..), UnOp(..))
import CoreImp.Constant (primModules)
import CoreImp.Misc (traverseLiteral)
import CoreImp.Module as CI
import Data.Array (concat, cons, difference, length, singleton, uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import PSString (PSString(..))

fnToImp ::
  forall m a.
  MonadThrow Error m =>
  CF.Module a -> m CI.Module
fnToImp (CF.Module m) = do
  stats <- concat <$> traverse decl m.moduleDecls
  let
    -- NOTE: remove self module and Prim modules
    imports =
      difference
        (map (\(CF.ModuleImport r) -> r.moduleName) m.moduleImports)
        (cons m.moduleName primModules)
  pure
    $ { moduleName: m.moduleName
      , moduleImports: imports
      , moduleExports: m.moduleExports
      , moduleForeigns: m.moduleForeign
      , moduleStats: stats
      }
  where
  decl :: CF.Bind a -> m (Array Stat)
  decl (CF.NonRec _ ident expr) = singleton <$> assign ident expr

  decl (CF.Rec vals) =
    traverse
      (\(Tuple (Tuple _ ident) expr) -> assign ident expr)
      vals

  assign :: CF.Ident -> CF.Expr a -> m Stat
  assign ident expr = Assign ident <$> toAST expr

  toAST :: CF.Expr a -> m Expr
  toAST (CF.Literal _ lit) = Literal <$> traverseLiteral toAST lit

  toAST (CF.Accessor _ k v) = Accessor (PSString k) <$> toAST v

  toAST (CF.ObjectUpdate _ obj kvs) = ObjectUpdate <$> toAST obj <*> traverse (traverse toAST) (lmap PSString <$> kvs)

  toAST (CF.Abs _ arg body) = Function arg <$> map return (toAST body)

  toAST (CF.App _ f x) = Apply <$> toAST f <*> toAST x

  toAST (CF.Var _ qi) = pure $ Variable qi

  toAST (CF.Case _ args alts) = iife <$> cases args alts

  toAST (CF.Let _ binds body) =
    iife
      <$> do
          binds' <- traverse decl binds
          body' <- toAST body
          pure $ concat binds' <> return body'

  toAST (CF.Constructor _ _ cn args) = pure $ Constructor cn args

  iife :: Array Stat -> Expr
  iife stats = Apply (Function (CF.Ident unusedVarName) stats) Unit
    where
    unusedVarName = "_unused_coreimp"

  return :: Expr -> Array Stat
  return = singleton <<< Return

  cases :: Array (CF.Expr a) -> Array (CF.CaseAlternative a) -> m (Array Stat)
  cases args alts = do
    vals <- traverse toAST args
    body <-
      traverse
        ( \(CF.CaseAlternative ca) -> do
            res <- guards ca.caseAlternativeResult
            go vals res ca.caseAlternativeBinders
        )
        alts
    pure $ concat body
    where
    guards :: Either (Array (Tuple (CF.Guard a) (CF.Expr a))) (CF.Expr a) -> m (Array Stat)
    guards (Left gs) =
      traverse
        (\(Tuple cond val) -> If <$> toAST cond <*> map return (toAST val))
        gs

    guards (Right v) = return <$> toAST v

    go :: Array Expr -> Array Stat -> Array (CF.Binder a) -> m (Array Stat)
    go _ done [] = pure done

    go vals' done binders = case uncons vals', uncons binders of
      Just { head: v, tail: vs }, Just { head: b, tail: bs } -> do
        done' <- go vs done bs
        binder v done' b
      _, _ -> throwError $ error "Invalid arguments to binders"

  binder :: Expr -> Array Stat -> CF.Binder a -> m (Array Stat)
  binder _ done (CF.NullBinder _) = pure done

  binder val done (CF.LiteralBinder _ l) = literalBinder val done l

  binder val done (CF.VarBinder _ ident) = pure $ [ Assign ident val ] <> done

  binder val done (CF.ConstructorBinder _ tn cn bs) = do
    stats <- go bs done
    pure [ If (TagOf cn val) stats ]
    where
    fieldCount = length bs

    go :: Array (CF.Binder a) -> Array Stat -> m (Array Stat)
    go binds done' = case uncons binds of
      Just { head: b, tail: bs' } -> do
        let
          -- NOTE: serial numbered value names (`value0`, `value1`, ...) according to corefn
          valueNumber = fieldCount - length bs' - 1

          acc = Accessor (PSString (fixedCtorArgName <> show valueNumber)) val
        done'' <- go bs' done'
        binder acc done'' b
      _ -> pure done'

  binder val done (CF.NamedBinder _ ident b) = do
    stats <- binder val done b
    pure $ [ Assign ident val ] <> stats

  literalBinder :: Expr -> Array Stat -> CF.Literal (CF.Binder a) -> m (Array Stat)
  literalBinder val done (CF.NumericLiteral n) = pure [ If (Binary Equal val (Literal (CF.NumericLiteral n))) done ]

  literalBinder val done (CF.CharLiteral c) = pure [ If (Binary Equal val (Literal (CF.CharLiteral c))) done ]

  literalBinder val done (CF.StringLiteral s) = pure [ If (Binary Equal val (Literal (CF.StringLiteral s))) done ]

  literalBinder val done (CF.BooleanLiteral true) = pure [ If val done ]

  literalBinder val done (CF.BooleanLiteral false) = pure [ If (Unary Not val) done ]

  literalBinder val done (CF.ObjectLiteral bs) = go done bs
    where
    go :: Array Stat -> Array (Tuple String (CF.Binder a)) -> m (Array Stat)
    go done' kvs = case uncons kvs of
      Just { head: Tuple prop b, tail: bs' } -> do
        let
          acc = Accessor (PSString prop) val
        done'' <- go done' bs'
        binder acc done'' b
      _ -> pure done'

  literalBinder val done (CF.ArrayLiteral bs) = do
    stats <- go done 0 bs
    pure [ If (Binary Equal (Unary Length val) (Literal (CF.NumericLiteral (Left (length bs))))) stats ]
    where
    go :: Array Stat -> Int -> Array (CF.Binder a) -> m (Array Stat)
    go done' index binds = case uncons binds of
      Just { head: b, tail: bs' } -> do
        let
          idx = Indexer index val
        done'' <- go done' (index + 1) bs'
        binder idx done'' b
      _ -> pure done'

fixedCtorArgName :: String
fixedCtorArgName = "value"
