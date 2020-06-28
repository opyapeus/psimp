module CodeGen.JS.Printer where

import Prelude
import CodeGen.JS.AST (BinOp(..), Expr(..), Lit(..), Stat(..), UnOp(..))
import Data.Foldable (intercalate)
import Data.Tuple (Tuple(..))
import Prettier.Printer (DOC, line, nest, pretty, text)

word ::
  { exports :: DOC
  , function :: DOC
  , if :: DOC
  , module :: DOC
  , require :: DOC
  , return :: DOC
  , throw :: DOC
  , var :: DOC
  }
word =
  { var: text "var"
  , function: text "function"
  , return: text "return"
  , if: text "if"
  , require: text "require"
  , throw: text "throw"
  , module: text "module"
  , exports: text "exports"
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

colon :: DOC
colon = text ":"

dot :: DOC
dot = text "."

stat :: Stat -> DOC
stat (VarAssign v x) = joinSpace [ word.var, text v, equal, expr x ]

stat (Assign v x) = joinSpace [ expr v, equal, expr x ]

stat (If cond stats) =
  joinSpace
    [ word.if
    , joinEmpty [ bracket.roundOpen, expr cond, bracket.roundClose ]
    , bracket.curlyOpen
    ]
    <> indent (line <> (intercalate line (map stat stats)))
    <> line
    <> bracket.curlyClose

stat (Return x) = joinSpace [ word.return, expr x ]

stat (Throw s) = joinSpace [ word.throw, text (show s) ]

stat (Export o) = joinSpace [ joinEmpty [ word.module, dot, word.exports ], equal, expr o ]

expr :: Expr -> DOC
expr (Var v) = text v

-- REVIEW: clean way to copy object
expr (Clone x) = text "JSON.parse(JSON.stringify(" <> expr x <> text "))"

expr (Literal l) = lit l

expr (Accessor a x) = joinEmpty [ expr x, dot, text a ]

expr (Indexer i x) = joinEmpty [ expr x, bracket.squareOpen, text (show i), bracket.squareClose ]

expr (App f x) = joinEmpty [ bracket.roundOpen, expr f, bracket.roundClose, bracket.roundOpen, expr x, bracket.roundClose ]

expr (Function arg stats) =
  joinSpace
    [ joinEmpty [ word.function, bracket.roundOpen, text arg, bracket.roundClose ]
    , bracket.curlyOpen
    ]
    <> indent (line <> (intercalate line (map stat stats)))
    <> line
    <> bracket.curlyClose

expr (Binary op a b) = joinSpace [ expr a, bin op, expr b ]

expr (Unary op x) = joinSpace [ un op, expr x ]

expr (Require x) = joinEmpty [ word.require, bracket.roundOpen, text (show x), bracket.roundClose ]

expr Null = text "null"

bin :: BinOp -> DOC
bin Eq = text "=="

bin Neq = text "!="

bin Gt = text ">"

bin Gte = text ">="

bin Lt = text "<"

bin Lte = text "<="

bin Add = text "+"

bin Sub = text "-"

bin Mul = text "*"

bin Div = text "/"

bin Mod = text "%"

bin And = text "&&"

bin Or = text "||"

un :: UnOp -> DOC
un Neg = text "-"

un Not = text "!"

lit :: Lit -> DOC
lit (Int i) = text $ show i

lit (Number n) = text $ show n

lit (Boolean b) = text $ show b

lit (String s) = text $ show s

lit (Array xs) =
  bracket.squareOpen
    <> intercalate comma (map expr xs)
    <> bracket.squareClose

lit (Object kvs) =
  bracket.curlyOpen
    <> indent
        ( intercalate comma
            $ map
                (\(Tuple k v) -> line <> joinSpace [ joinEmpty [ text k, colon ], expr v ])
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
