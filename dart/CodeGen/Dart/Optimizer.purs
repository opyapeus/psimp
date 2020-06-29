module CodeGen.Dart.Optimizer where

import Prelude
import CodeGen.Dart.AST (Expr(..), Stat(..), everywhere)

optimize :: Stat -> Stat
optimize = enhance <<< necessary
  where
  necessary = functionize

  enhance = identity

-- NOTE: this is necessary to define recursive function
functionize :: Stat -> Stat
functionize = everywhere identity convert
  where
  convert (VarAssign name (Function arg stats)) = FunctionDecl name arg stats

  convert a = a
