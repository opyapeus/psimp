module CoreImp.AST where

import Prelude
import CoreFn.Ident (Ident) as CF
import CoreFn.Literal (Literal(..)) as CF
import CoreFn.Names (Qualified) as CF
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import PSString (PSString)

-- Imperative core AST for languages
-- - no need to declare type explicitly
-- - support currying
-- - have garbage collection
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
  -- | Anonimous function (argument name, statements)
  | Function CF.Ident (Array Stat)
  -- | Binary operation
  | Binary BinOp Expr Expr
  -- | Unary operation
  | Unary UnOp Expr
  -- | Unit (Prim.undefined)
  | Unit

data Stat
  -- | Assignment
  = Assign CF.Ident Expr
  -- | Update assignment
  | UpdateAssign Expr Expr
  -- | Create ocject clone
  | ObjectCopy CF.Ident Expr
  -- | Conditional statement
  | If Expr (Array Stat)
  -- | Return value
  | Return Expr

instance showExpr :: Show Expr where
  show (Literal lit) = showCtor "Literal" [ show lit ]
  show (Accessor k v) = showCtor "Accessor" [ show k, show v ]
  show (Indexer i v) = showCtor "Indexer" [ show i, show v ]
  show (Apply f x) = showCtor "Apply" [ show f, show x ]
  show (Variable qi) = showCtor "Variable" [ show qi ]
  show (Function arg stats) = showCtor "Function" [ show arg, show stats ]
  show (Binary op x y) = showCtor "Binary" [ show op, show x, show y ]
  show (Unary op x) = showCtor "Unary" [ show op, show x ]
  show Unit = "Unit"

instance showStat :: Show Stat where
  show (Assign ident val) = showCtor "Assign" [ show ident, show val ]
  show (UpdateAssign obj new) = showCtor "UpdateAssign" [ show obj, show new ]
  show (ObjectCopy ident obj) = showCtor "ObjectCopy" [ show ident, show obj ]
  show (If cond stats) = showCtor "If" [ show cond, show stats ]
  show (Return val) = showCtor "Return" [ show val ]

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
  | Length
  | Not

derive instance genericBinOp :: Generic BinOp _

derive instance genericUnOp :: Generic UnOp _

instance showBinOp :: Show BinOp where
  show = genericShow

instance showUnOp :: Show UnOp where
  show = genericShow

everywhere :: (Expr -> Expr) -> (Stat -> Stat) -> Stat -> Stat
everywhere f g = go'
  where
  go :: Expr -> Expr
  go (Literal lit) = f $ Literal (go'' lit)

  go (Accessor a x) = f $ Accessor a (go x)

  go (Indexer i x) = f $ Indexer i (go x)

  go (Apply f' x) = f $ Apply (go f') (go x)

  go (Function arg stats) = f $ Function arg (map go' stats)

  go (Binary op x y) = f $ Binary op (go x) (go y)

  go (Unary op x) = f $ Unary op (go x)

  go other = f other

  go' :: Stat -> Stat
  go' (Assign ident var) = g $ Assign ident (go var)

  go' (UpdateAssign obj new) = g $ UpdateAssign (go obj) (go new)

  go' (ObjectCopy ident var) = g $ ObjectCopy ident (go var)

  go' (If cond stats) = g $ If (go cond) (map go' stats)

  go' (Return x) = g $ Return (go x)

  go'' :: CF.Literal Expr -> CF.Literal Expr
  go'' (CF.ArrayLiteral xs) = CF.ArrayLiteral (map go xs)

  go'' (CF.ObjectLiteral kvs) = CF.ObjectLiteral (map (map go) kvs)

  go'' other = other
