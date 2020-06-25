module CodeGen.Lua.Printer where

import Prelude
import CodeGen.Lua.AST (BinOp(..), Expr(..), Lit(..), Stat(..), UnOp(..))
import Data.Foldable (intercalate)
import Data.Tuple (Tuple(..))
import Prettier.Printer (DOC, line, nest, pretty, text)

word ::
  { else :: DOC
  , end :: DOC
  , function :: DOC
  , if :: DOC
  , local :: DOC
  , require :: DOC
  , return :: DOC
  , then :: DOC
  }
word =
  { local: text "local"
  , function: text "function"
  , return: text "return"
  , end: text "end"
  , if: text "if"
  , then: text "then"
  , else: text "else"
  , require: text "require"
  }

bracket ::
  { curlyClose :: DOC
  , curlyOpen :: DOC
  , roundClose :: DOC
  , roundOpen :: DOC
  , squareClose :: DOC
  , squareOpen :: DOC
  }
bracket =
  { roundOpen: text "("
  , roundClose: text ")"
  , curlyOpen: text "{"
  , curlyClose: text "}"
  , squareOpen: text "["
  , squareClose: text "]"
  }

equal :: DOC
equal = text "="

comma :: DOC
comma = text ","

dot :: DOC
dot = text "."

stat :: Stat -> DOC
stat (LocalAssign v x) = joinSpace [ word.local, text v, equal, expr x ]

stat (Assign v x) = joinSpace [ expr v, equal, expr x ]

stat (If cond stats) =
  joinSpace [ word.if, expr cond, word.then ]
    <> indent (line <> (intercalate line (map stat stats)))
    <> line
    <> word.end

stat (Return x) = joinSpace [ word.return, expr x ]

expr :: Expr -> DOC
expr (Var v) = text v

expr (Literal l) = lit l

expr (Accessor a x) = joinEmpty [ expr x, dot, text a ]

expr (Indexer i x) = joinEmpty [ expr x, bracket.squareOpen, text (show i), bracket.squareClose ]

expr (App f x) = joinEmpty [ bracket.roundOpen, expr f, bracket.roundClose, bracket.roundOpen, expr x, bracket.roundClose ]

expr (Function arg stats) =
  word.function
    <> bracket.roundOpen
    <> text arg
    <> bracket.roundClose
    <> indent (line <> (intercalate line (map stat stats)))
    <> line
    <> word.end

expr (Binary op a b) = joinSpace [ expr a, bin op, expr b ]

expr (Unary op x) = joinSpace [ un op, expr x ]

expr (Require x) = joinSpace [ word.require, text (show x) ]

expr Nil = text "nil"

bin :: BinOp -> DOC
bin Eq = text "=="

bin Neq = text "~="

bin Gt = text ">"

bin Gte = text ">="

bin Lt = text "<"

bin Lte = text "<="

bin Add = text "+"

bin Sub = text "-"

bin Mul = text "*"

bin Div = text "/"

bin Mod = text "%"

bin And = text "and"

bin Or = text "or"

un :: UnOp -> DOC
un Neg = text "-"

un Len = text "#"

un Not = text "not"

lit :: Lit -> DOC
lit (Int i) = text $ show i

lit (Number n) = text $ show n

lit (Boolean b) = text $ show b

lit (String s) = text $ show s

lit (Array xs) =
  bracket.curlyOpen
    <> intercalate comma (map expr xs)
    <> bracket.curlyClose

lit (Object kvs) =
  bracket.curlyOpen
    <> indent
        ( intercalate comma
            $ map
                (\(Tuple k v) -> line <> joinSpace [ text k, equal, expr v ])
                kvs
        )
    <> line
    <> bracket.curlyClose

joinSpace :: Array DOC -> DOC
joinSpace = intercalate (text " ")

joinEmpty :: Array DOC -> DOC
joinEmpty = intercalate (text "")

indent :: DOC -> DOC
indent = nest 4

-- NOTE: pretty each stat because large DOC sometime makes stack overflow for now...
print :: Array Stat -> String
print = intercalate "\n" <<< map (pretty top <<< statEndWithLine)
  where
  statEndWithLine s = stat s <> line
