module CodeGen.Go.AST where

import Prelude
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

data Stat
  = VarAssign String Expr
  | Assign Expr Expr
  | If Expr (Array Stat)
  | Return Expr
  | Import (Array (Tuple (Maybe Prefix) String))
  | Package String
  | Throw String
  | Raw String

data Expr
  = Var String
  | Clone Expr
  | Literal Lit
  | Reference String Expr
  | Accessor String Expr
  | Indexer Int Expr
  | App Expr Expr
  | Function String (Array Stat)
  | Binary BinOp Expr Expr
  | Unary UnOp Expr
  | Length Expr
  | Nil

data BinOp
  = Eq
  | Neq
  | Gt
  | Gte
  | Lt
  | Lte
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or

data UnOp
  = Neg
  | Len
  | Not

data Lit
  = Int Int
  | Number Number
  | Boolean Boolean
  | String String
  | Array (Array Expr)
  | Object (Array (Tuple String Expr))

data Prefix
  = Dot

instance showStat :: Show Stat where
  show (VarAssign a b) = showCtor "VarAssign" [ show a, show b ]
  show (Assign a b) = showCtor "Assign" [ show a, show b ]
  show (If cond stats) = showCtor "If" [ show cond, show stats ]
  show (Return x) = showCtor "Return" [ show x ]
  show (Import paths) = showCtor "Import" [ show paths ]
  show (Package name) = showCtor "Package" [ show name ]
  show (Throw s) = showCtor "Throw" [ show s ]
  show (Raw s) = showCtor "Raw" [ show s ]

instance showExpr :: Show Expr where
  show (Var x) = showCtor "Var" [ show x ]
  show (Clone x) = showCtor "Clone" [ show x ]
  show (Literal lit) = showCtor "Literal" [ show lit ]
  show (Reference a x) = showCtor "Reference" [ show a, show x ]
  show (Accessor a x) = showCtor "Accessor" [ show a, show x ]
  show (Indexer i x) = showCtor "Indexer" [ show i, show x ]
  show (App f x) = showCtor "App" [ show f, show x ]
  show (Function arg stats) = showCtor "Function" [ show arg, show stats ]
  show (Binary op x y) = showCtor "Binary" [ show op, show x, show y ]
  show (Unary op x) = showCtor "Unary" [ show op, show x ]
  show (Length arr) = showCtor "Length" [ show arr ]
  show Nil = "Nil"

instance showLit :: Show Lit where
  show (Int i) = showCtor "Int" [ show i ]
  show (Number n) = showCtor "Number" [ show n ]
  show (Boolean b) = showCtor "Boolean" [ show b ]
  show (String s) = showCtor "String" [ show s ]
  show (Array xs) = showCtor "Array" [ show xs ]
  show (Object kvs) = showCtor "Object" [ show kvs ]

showCtor :: String -> Array String -> String
showCtor name args =
  "("
    <> name
    <> " "
    <> intercalate " " args
    <> ")"

derive instance genericBinOp :: Generic BinOp _

derive instance genericUnOp :: Generic UnOp _

derive instance genericPrefix :: Generic Prefix _

instance showBinOp :: Show BinOp where
  show = genericShow

instance showUnOp :: Show UnOp where
  show = genericShow

instance showPrefix :: Show Prefix where
  show = genericShow

everywhere :: (Expr -> Expr) -> Stat -> Stat
everywhere f = go'
  where
  go :: Expr -> Expr
  go (Literal lit) = f (Literal (go'' lit))

  go (Accessor a x) = f (Accessor a (go x))

  go (App f' x) = f (App (go f') (go x))

  go (Function arg stats) = f (Function arg (map go' stats))

  go (Binary op x y) = f (Binary op (go x) (go y))

  go (Unary op x) = f (Unary op (go x))

  go other = f other

  go' :: Stat -> Stat
  go' (Assign a b) = (Assign (go a) (go b))

  go' (If cond stats) = (If (go cond) (map go' stats))

  go' (Return x) = (Return (go x))

  go' other = other

  go'' :: Lit -> Lit
  go'' (Array xs) = (Array (map go xs))

  go'' (Object kvs) = (Object (map (map go) kvs))

  go'' other = other
