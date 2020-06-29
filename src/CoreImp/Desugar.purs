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
  decl :: CF.Bind CF.Ann -> m (Array Stat)
  decl (CF.NonRec _ ident expr) = singleton <$> assign ident expr

  decl (CF.Rec vals) =
    traverse
      (\(Tuple (Tuple _ ident) expr) -> assign ident expr)
      vals

  assign :: CF.Ident -> CF.Expr CF.Ann -> m Stat
  assign ident expr = Assign ident <$> toAST expr

  toAST :: CF.Expr CF.Ann -> m Expr
  toAST (CF.Literal _ lit) = Literal <$> traverseLiteral toAST lit

  toAST (CF.Accessor _ k v) = Accessor (PSString k) <$> toAST v

  toAST (CF.ObjectUpdate _ obj kvs) =
    iife
      <$> do
          obj' <- toAST obj
          kvs' <- traverse (traverse toAST) kvs
          pure
            $ [ ObjectCopy resultIdent obj' ]
            <> map (\(Tuple k v) -> UpdateAssign (Accessor (PSString k) (Variable (CF.Qualified Nothing resultIdent))) v) kvs'
            <> [ Return (Variable (CF.Qualified Nothing resultIdent)) ]
    where
    resultIdent = CF.Ident "_ci_obj"

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

  toAST (CF.Constructor _ _ cn args) = go args
    where
    go :: Array CF.Ident -> m Expr
    go idents = case uncons idents of
      Just { head: ident, tail: remain } ->
        Function ident
          <<< singleton
          <<< Return
          <$> go remain
      Nothing ->
        Literal
          <<< CF.ObjectLiteral
          <<< append [ Tuple fixedCtorTagName (Literal (CF.StringLiteral (unProper cn))) ]
          <$> traverse (\arg -> Tuple <$> unIdent arg <@> Variable (CF.Qualified Nothing arg)) args

  iife :: Array Stat -> Expr
  iife stats = Apply (Function (CF.Ident unusedVarName) stats) Unit
    where
    unusedVarName = "_ci_unused"

  return :: Expr -> Array Stat
  return = singleton <<< Return

  cases :: Array (CF.Expr CF.Ann) -> Array (CF.CaseAlternative CF.Ann) -> m (Array Stat)
  cases args alts = do
    vals <- traverse toAST args
    body <-
      traverse
        ( \(CF.CaseAlternative ca) -> do
            res <- guards ca.caseAlternativeResult
            go vals res ca.caseAlternativeBinders
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

    go vals' done binders = case uncons vals', uncons binders of
      Just { head: v, tail: vs }, Just { head: b, tail: bs } -> do
        done' <- go vs done bs
        binder v done' b
      _, _ -> throwError $ error "Invalid arguments to binders"

  binder :: Expr -> Array Stat -> CF.Binder CF.Ann -> m (Array Stat)
  binder _ done (CF.NullBinder _) = pure done

  binder val done (CF.LiteralBinder _ l) = literalBinder val done l

  binder val done (CF.VarBinder _ ident) = pure $ [ Assign ident val ] <> done

  -- NOTE: Newtype is treated as Abs in CoreFn
  binder val done (CF.ConstructorBinder (CF.Ann { sourceSpan: _, comments: _, type: _, meta: Just CF.IsNewtype }) _ _ [ b ]) = binder val done b

  binder val done (CF.ConstructorBinder _ _ (CF.Qualified _ cn) bs) = do
    stats <- go bs done
    pure
      [ If
          ( Binary
              Equal
              (Accessor (PSString fixedCtorTagName) val)
              (Literal (CF.StringLiteral (unProper cn)))
          )
          stats
      ]
    where
    fieldCount = length bs

    go :: Array (CF.Binder CF.Ann) -> Array Stat -> m (Array Stat)
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

  literalBinder :: Expr -> Array Stat -> CF.Literal (CF.Binder CF.Ann) -> m (Array Stat)
  literalBinder val done (CF.NumericLiteral n) = pure [ If (Binary Equal val (Literal (CF.NumericLiteral n))) done ]

  literalBinder val done (CF.CharLiteral c) = pure [ If (Binary Equal val (Literal (CF.CharLiteral c))) done ]

  literalBinder val done (CF.StringLiteral s) = pure [ If (Binary Equal val (Literal (CF.StringLiteral s))) done ]

  literalBinder val done (CF.BooleanLiteral b) = pure [ If (Binary Equal val (Literal (CF.BooleanLiteral b))) done ]

  literalBinder val done (CF.ObjectLiteral bs) = go done bs
    where
    go :: Array Stat -> Array (Tuple String (CF.Binder CF.Ann)) -> m (Array Stat)
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
    go :: Array Stat -> Int -> Array (CF.Binder CF.Ann) -> m (Array Stat)
    go done' index binds = case uncons binds of
      Just { head: b, tail: bs' } -> do
        let
          idx = Indexer index val
        done'' <- go done' (index + 1) bs'
        binder idx done'' b
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

unIdent CF.UnusedIdent = throwError $ error "GenIdent is not supported."
