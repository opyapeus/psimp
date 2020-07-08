module CoreImp.AST where

import Prelude
import CoreFn.Ident (Ident) as CF
import CoreFn.Literal (Literal(..)) as CF
import CoreFn.Names (Qualified) as CF
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import PSString (PSString)

-- Imperative Core AST
data Expr
  -- | Literal value
  = Literal (CF.Literal Expr)
  -- | Object accessor
  | Accessor PSString Expr
  -- | Array accessor
  | Indexer Int Expr
  -- | Function application
  | Apply Expr Expr
  -- | Variable
  | Variable (CF.Qualified CF.Ident)
  -- | Anonimous function (single argument, statements)
  | Function CF.Ident (Array Stat)
  -- | Binary operation
  | Binary BinOp Expr Expr
  -- | Unary operation
  | Unary UnOp Expr
  -- | Object clone
  | ObjectClone Expr
  -- | Array length
  | ArrayLength Expr
  -- | Unit
  | Unit

data Stat
  -- | Assignment
  = Assign CF.Ident Expr
  -- | Overwrite assignment
  | UpdateAssign Expr Expr
  -- | Conditional statement
  | If Expr (Array Stat)
  -- | Return value
  | Return Expr
  -- | Throw error
  | Throw String

instance showExpr :: Show Expr where
  show (Literal l) = showCtor "Literal" [ show l ]
  show (Accessor k v) = showCtor "Accessor" [ show k, show v ]
  show (Indexer i v) = showCtor "Indexer" [ show i, show v ]
  show (Apply f x) = showCtor "Apply" [ show f, show x ]
  show (Variable qi) = showCtor "Variable" [ show qi ]
  show (Function arg stats) = showCtor "Function" [ show arg, show stats ]
  show (Binary op x y) = showCtor "Binary" [ show op, show x, show y ]
  show (Unary op x) = showCtor "Unary" [ show op, show x ]
  show (ObjectClone o) = showCtor "ObjectClone" [ show o ]
  show (ArrayLength arr) = showCtor "ArrayLength" [ show arr ]
  show Unit = "Unit"

instance showStat :: Show Stat where
  show (Assign ident val) = showCtor "Assign" [ show ident, show val ]
  show (UpdateAssign obj val) = showCtor "UpdateAssign" [ show obj, show val ]
  show (If cond stats) = showCtor "If" [ show cond, show stats ]
  show (Return val) = showCtor "Return" [ show val ]
  show (Throw s) = showCtor "Throw" [ show s ]

showCtor :: String -> Array String -> String
showCtor name args =
  "("
    <> name
    <> " "
    <> intercalate " " args
    <> ")"

data BinOp
  = Equal
  | NotEqual
  | GreaterThan
  | GreaterThanEqual
  | LessThan
  | LessThanEqual
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulus
  | And
  | Or

data UnOp
  = Negative
  | Not

derive instance genericBinOp :: Generic BinOp _

derive instance genericUnOp :: Generic UnOp _

instance showBinOp :: Show BinOp where
  show = genericShow

instance showUnOp :: Show UnOp where
  show = genericShow

everywhere :: (Expr -> Expr) -> (Stat -> Stat) -> Stat -> Stat
everywhere fE fS = goS
  where
  goE :: Expr -> Expr
  goE (Literal l) = fE $ Literal (goL l)

  goE (Accessor a x) = fE $ Accessor a (goE x)

  goE (Indexer i x) = fE $ Indexer i (goE x)

  goE (Apply f' x) = fE $ Apply (goE f') (goE x)

  goE (Function arg stats) = fE $ Function arg (map goS stats)

  goE (Binary op x y) = fE $ Binary op (goE x) (goE y)

  goE (Unary op x) = fE $ Unary op (goE x)

  goE (ObjectClone o) = fE $ ObjectClone (goE o)

  goE (ArrayLength arr) = fE $ ArrayLength (goE arr)

  goE other = fE other

  goS :: Stat -> Stat
  goS (Assign ident var) = fS $ Assign ident (goE var)

  goS (UpdateAssign obj new) = fS $ UpdateAssign (goE obj) (goE new)

  goS (If cond stats) = fS $ If (goE cond) (map goS stats)

  goS (Return x) = fS $ Return (goE x)

  goS other = fS other

  goL :: CF.Literal Expr -> CF.Literal Expr
  goL (CF.ArrayLiteral xs) = CF.ArrayLiteral (map goE xs)

  goL (CF.ObjectLiteral kvs) = CF.ObjectLiteral (map (map goE) kvs)

  goL other = other
