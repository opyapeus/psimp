module CoreImp.AST where

import Prelude
import CoreFn.Ident (Ident) as CF
import CoreFn.Literal (Literal(..)) as CF
import CoreFn.Names (ProperName, Qualified) as CF
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple (Tuple)
import PSString (PSString)

data Expr a
  -- | A literal value
  = Literal (CF.Literal (Expr a))
  -- | A data constructor (constructor name, field names)  
  | Constructor CF.ProperName (Array CF.Ident)
  -- | An object accessor
  | Accessor PSString (Expr a)
  -- | An array accessor
  | Indexer Int (Expr a)
  -- | Partial object update
  | ObjectUpdate (Expr a) (Array (Tuple PSString (Expr a)))
  -- | Function application
  | Apply (Expr a) (Expr a)
  -- | Variable
  | Variable (CF.Qualified CF.Ident)
  -- | Anonimous function (argument name, statements)
  | Function CF.Ident (Array (Stat a))
  -- | Let In
  | LetIn (Array (Stat a))
  -- | Binary operation
  | Binary BinOp (Expr a) (Expr a)
  -- | Unary operation
  | Unary UnOp (Expr a)
  -- | Tag Of
  | TagOf (CF.Qualified CF.ProperName) (Expr a)
  -- | Unit (Prim.undefined)
  | Unit

data Stat a
  -- | Assignment
  = Assign CF.Ident (Expr a)
  -- | Conditional statement
  | If (Expr a) (Array (Stat a))
  -- | Return value
  | Return (Expr a)

derive instance functorExpr :: Functor Expr

derive instance functorStat :: Functor Stat

instance showExpr :: Show a => Show (Expr a) where
  show (Literal lit) = showCtor "Literal" [ show lit ]
  show (Constructor cn args) = showCtor "Constructor" [ show cn, show args ]
  show (Accessor k v) = showCtor "Accessor" [ show k, show v ]
  show (Indexer i v) = showCtor "Indexer" [ show i, show v ]
  show (ObjectUpdate obj kvs) = showCtor "ObjectUpdate" [ show obj, show kvs ]
  show (Apply f x) = showCtor "Apply" [ show f, show x ]
  show (Variable qi) = showCtor "Variable" [ show qi ]
  show (Function arg stats) = showCtor "Function" [ show arg, show stats ]
  show (LetIn stats) = showCtor "LetIn" [ show stats ]
  show (Binary op x y) = showCtor "Binary" [ show op, show x, show y ]
  show (Unary op x) = showCtor "Unary" [ show op, show x ]
  show (TagOf cn x) = showCtor "TagOf" [ show cn, show x ]
  show Unit = "Unit"

instance showStat :: Show a => Show (Stat a) where
  show (Assign ident val) = showCtor "Assign" [ show ident, show val ]
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

everywhere :: forall a. (Expr a -> Expr a) -> Stat a -> Stat a
everywhere f = go'
  where
  go :: Expr a -> Expr a
  go (Literal lit) = f (Literal (go'' lit))

  go (Accessor a x) = f (Accessor a (go x))

  go (Indexer i x) = f (Indexer i (go x))

  go (ObjectUpdate obj kvs) = f (ObjectUpdate (go obj) (map (map go) kvs))

  go (Apply f' x) = f (Apply (go f') (go x))

  go (Function arg stats) = f (Function arg (map go' stats))

  go (LetIn stats) = f (LetIn (map go' stats))

  go (Binary op x y) = f (Binary op (go x) (go y))

  go (Unary op x) = f (Unary op (go x))

  go (TagOf cn x) = f (TagOf cn (go x))

  go other = f other

  go' :: Stat a -> Stat a
  go' (Assign ident var) = (Assign ident (go var))

  go' (If cond stats) = (If (go cond) (map go' stats))

  go' (Return x) = (Return (go x))

  go'' :: CF.Literal (Expr a) -> CF.Literal (Expr a)
  go'' (CF.ArrayLiteral xs) = (CF.ArrayLiteral (map go xs))

  go'' (CF.ObjectLiteral kvs) = (CF.ObjectLiteral (map (map go) kvs))

  go'' other = other
