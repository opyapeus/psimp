module CoreImp.Desugar where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import CoreFn.Ann (Ann(..)) as CF
import CoreFn.Binders (Binder(..)) as CF
import CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..), Guard) as CF
import CoreFn.Ident (Ident(..)) as CF
import CoreFn.Literal (Literal(..)) as CF
import CoreFn.Meta (Meta(..)) as CF
import CoreFn.Module (Module(..), ModuleImport(..)) as CF
import CoreFn.Names (ProperName(..), Qualified(..)) as CF
import CoreImp.AST (BinOp(..), Expr(..), Stat(..), UnOp(..))
import CoreImp.Constant (primModules)
import CoreImp.Misc (traverseLiteral)
import CoreImp.Module as CI
import Data.Array (concat, cons, difference, length, singleton, uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import PSString (PSString(..))

fnToImp ::
  forall m.
  MonadThrow Error m =>
  CF.Module CF.Ann -> m CI.Module
fnToImp (CF.Module m) = do
  stats <- concat <$> traverse decl m.moduleDecls
  pure
    { moduleName: m.moduleName
    , moduleImports: imports
    , moduleExports: m.moduleExports
    , moduleForeigns: m.moduleForeign
    , moduleStats: stats
    }
  where
  -- NOTE: remove self module and Prim modules
  imports =
    difference
      (map (\(CF.ModuleImport r) -> r.moduleName) m.moduleImports)
      (cons m.moduleName primModules)

  decl :: CF.Bind CF.Ann -> m (Array Stat)
  decl (CF.NonRec _ ident expr) = singleton <$> assign ident expr

  decl (CF.Rec vals) =
    traverse
      (\(Tuple (Tuple _ ident) expr) -> assign ident expr)
      vals

  assign :: CF.Ident -> CF.Expr CF.Ann -> m Stat
  assign ident expr = Assign ident <$> toAST expr

  toAST :: CF.Expr CF.Ann -> m Expr
  toAST (CF.Literal _ l) = Literal <$> traverseLiteral toAST l

  toAST (CF.Constructor _ _ cn args) = go args
    where
    go :: Array CF.Ident -> m Expr
    go idents = case uncons idents of
      Just { head: ident, tail: remain } -> Function ident <<< return <$> go remain
      Nothing ->
        Literal
          <<< CF.ObjectLiteral
          <<< cons (Tuple fixedCtorTagName (Literal (CF.StringLiteral (unProper cn)))) -- NOTE: add tag field to object to identify constructor 
          <$> traverse (\arg -> Tuple <$> unIdent arg <@> Variable (CF.Qualified Nothing arg)) args

  toAST (CF.Accessor _ k v) = Accessor (PSString k) <$> toAST v

  toAST o@(CF.ObjectUpdate _ obj _) = do
    res <- flatten identity o
    obj' <- toAST obj
    pure <<< iife
      <<< cons (Assign resultIdent (ObjectClone obj'))
      $ map (\(Tuple acc v) -> UpdateAssign (acc resultVar) v) res
      <> return resultVar
    where
    resultIdent = CF.Ident "_ci_obj"

    resultVar = Variable (CF.Qualified Nothing resultIdent)

    flatten :: (Expr -> Expr) -> CF.Expr CF.Ann -> m (Array (Tuple (Expr -> Expr) Expr))
    flatten acc (CF.ObjectUpdate _ _ kvs) = concat <$> traverse mk kvs
      where
      mk (Tuple k v) = flatten (Accessor (PSString k) <<< acc) v

    flatten acc v = singleton <$> Tuple acc <$> toAST v

  toAST (CF.Abs _ arg x) = Function arg <$> return <$> toAST x

  toAST (CF.App _ f x) = Apply <$> toAST f <*> toAST x

  toAST (CF.Var _ qi) = pure $ Variable qi

  toAST (CF.Case _ args alts) = iife <$> cases args alts

  toAST (CF.Let _ binds x) = do
    statss <- traverse decl binds
    res <- toAST x
    pure <<< iife $ concat statss <> return res

  iife :: Array Stat -> Expr
  iife stats = Apply (Function (CF.Ident unusedVarName) stats) Unit
    where
    unusedVarName = "_ci_unused"

  return :: Expr -> Array Stat
  return = singleton <<< Return

  ifEqual :: Expr -> Expr -> Array Stat -> Stat
  ifEqual a b = If (Binary Equal a b)

  cases :: Array (CF.Expr CF.Ann) -> Array (CF.CaseAlternative CF.Ann) -> m (Array Stat)
  cases args alts = do
    vals <- traverse toAST args
    body <-
      traverse
        ( \(CF.CaseAlternative ca) -> do
            done <- guards ca.caseAlternativeResult
            go vals done ca.caseAlternativeBinders
        )
        alts
    pure
      $ concat body
      <> [ Throw "Failed to pattern match." ] -- NOTE: this is removed in optimization when it is after return.
    where
    guards :: Either (Array (Tuple (CF.Guard CF.Ann) (CF.Expr CF.Ann))) (CF.Expr CF.Ann) -> m (Array Stat)
    guards (Left gs) =
      traverse
        (\(Tuple cond val) -> If <$> toAST cond <*> map return (toAST val))
        gs

    guards (Right v) = return <$> toAST v

    go :: Array Expr -> Array Stat -> Array (CF.Binder CF.Ann) -> m (Array Stat)
    go _ done [] = pure done

    go vals done binders = case uncons vals, uncons binders of
      Just { head: v, tail: vs }, Just { head: b, tail: bs } -> do
        done' <- go vs done bs
        binder v done' b
      _, _ -> throwError $ error "Invalid arguments to binders"

  binder :: Expr -> Array Stat -> CF.Binder CF.Ann -> m (Array Stat)
  binder _ done (CF.NullBinder _) = pure done

  binder val done (CF.LiteralBinder _ l) = literalBinder val done l

  binder val done (CF.VarBinder _ ident) = pure $ cons (Assign ident val) done

  -- NOTE: Newtype is treated as Abs in CoreFn
  binder val done (CF.ConstructorBinder (CF.Ann { sourceSpan: _, comments: _, type: _, meta: Just CF.IsNewtype }) _ _ [ b ]) = binder val done b

  binder val done (CF.ConstructorBinder _ _ (CF.Qualified _ cn) bs) = do
    stats <- go bs done
    pure <<< singleton
      $ ifEqual
          (Accessor (PSString fixedCtorTagName) val)
          (Literal (CF.StringLiteral (unProper cn)))
          stats
    where
    fieldCount = length bs

    go :: Array (CF.Binder CF.Ann) -> Array Stat -> m (Array Stat)
    go binds done' = case uncons binds of
      Just { head: b, tail: bs' } -> do
        let
          -- NOTE: serial numbered value names (`value0`, `value1`, ...) according to CoreFn
          valueNumber = fieldCount - length bs' - 1

          acc = Accessor (PSString (fixedCtorArgName <> show valueNumber)) val
        done'' <- go bs' done'
        binder acc done'' b
      _ -> pure done'

  binder val done (CF.NamedBinder _ ident b) = do
    stats <- binder val done b
    pure $ cons (Assign ident val) stats

  literalBinder :: Expr -> Array Stat -> CF.Literal (CF.Binder CF.Ann) -> m (Array Stat)
  literalBinder val done (CF.NumericLiteral n) = pure <<< singleton $ ifEqual val (Literal (CF.NumericLiteral n)) done

  literalBinder val done (CF.CharLiteral c) = pure <<< singleton $ ifEqual val (Literal (CF.CharLiteral c)) done

  literalBinder val done (CF.StringLiteral s) = pure <<< singleton $ ifEqual val (Literal (CF.StringLiteral s)) done

  literalBinder val done (CF.BooleanLiteral true) = pure <<< singleton $ If val done

  literalBinder val done (CF.BooleanLiteral false) = pure <<< singleton $ If (Unary Not val) done

  literalBinder val done (CF.ObjectLiteral bs) = go done bs
    where
    go :: Array Stat -> Array (Tuple String (CF.Binder CF.Ann)) -> m (Array Stat)
    go done' pbs = case uncons pbs of
      Just { head: Tuple prop b, tail: pbs' } -> do
        done'' <- go done' pbs'
        binder acc done'' b
        where
        acc = Accessor (PSString prop) val
      _ -> pure done'

  literalBinder val done (CF.ArrayLiteral bs) = do
    stats <- go done 0 bs
    pure <<< singleton
      $ ifEqual
          (ArrayLength val)
          (Literal (CF.NumericLiteral (Left (length bs))))
          stats
    where
    go :: Array Stat -> Int -> Array (CF.Binder CF.Ann) -> m (Array Stat)
    go done' index binds = case uncons binds of
      Just { head: b, tail: bs' } -> do
        done'' <- go done' (index + 1) bs'
        binder idx done'' b
        where
        idx = Indexer index val
      _ -> pure done'

-- NOTE: CoreFn specific
fixedCtorArgName :: String
fixedCtorArgName = "value"

-- NOTE: this library specific
fixedCtorTagName :: String
fixedCtorTagName = "tag"

unProper :: CF.ProperName -> String
unProper (CF.ProperName s) = s

unIdent :: forall m. MonadThrow Error m => CF.Ident -> m String
unIdent (CF.Ident s) = pure s

unIdent (CF.GenIdent _ _) = throwError $ error "GenIdent is not supported."

unIdent CF.UnusedIdent = throwError $ error "UnusedIdent is not supported."
