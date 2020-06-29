module CodeGen.Dart.Printer where

import Prelude
import CodeGen.Dart.AST (BinOp(..), Expr(..), Lit(..), Stat(..), UnOp(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prettier.Printer (DOC, line, nest, pretty, text)

word ::
  { as :: DOC
  , if :: DOC
  , import :: DOC
  , length :: DOC
  , require :: DOC
  , return :: DOC
  , throw :: DOC
  , var :: DOC
  }
word =
  { var: text "var"
  , return: text "return"
  , if: text "if"
  , require: text "require"
  , throw: text "throw"
  , import: text "import"
  , as: text "as"
  , length: text "length"
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

semicolon :: DOC
semicolon = text ";"

dot :: DOC
dot = text "."

stat :: Stat -> DOC
stat (VarAssign v x) = joinSpace [ word.var, text v, equal, expr x ] <> semicolon

stat (Assign v x) = joinSpace [ expr v, equal, expr x ] <> semicolon

stat (If cond stats) =
  joinSpace
    [ word.if
    , joinEmpty [ bracket.roundOpen, expr cond, bracket.roundClose ]
    , bracket.curlyOpen
    ]
    <> indent (line <> (intercalate line (map stat stats)))
    <> line
    <> bracket.curlyClose

stat (Return x) = joinSpace [ word.return, expr x ] <> semicolon

stat (Throw s) = joinSpace [ word.throw, text (show s) ] <> semicolon

stat (Import s (Just as)) = joinSpace [ word.import, text (show s), word.as, text as ] <> semicolon

stat (Import s Nothing) = joinSpace [ word.import, text (show s) ] <> semicolon

stat (FunctionDecl name arg stats) =
  joinSpace
    [ joinEmpty [ text name, bracket.roundOpen, text arg, bracket.roundClose ]
    , bracket.curlyOpen
    ]
    <> indent (line <> (intercalate line (map stat stats)))
    <> line
    <> bracket.curlyClose

expr :: Expr -> DOC
expr (Var v) = text v

-- REVIEW: clean way to copy object
expr (Clone x) = text "new Map<String, Object>.from(" <> expr x <> text ")"

expr (Literal l) = lit l

expr (Referrer m v) = joinEmpty [ text m, dot, text v ]

expr (Accessor a x) = joinEmpty [ expr x, bracket.squareOpen, text (show a), bracket.squareClose ]

expr (Indexer i x) = joinEmpty [ expr x, bracket.squareOpen, text (show i), bracket.squareClose ]

expr (App f x) = joinEmpty [ bracket.roundOpen, expr f, bracket.roundClose, bracket.roundOpen, expr x, bracket.roundClose ]

expr (Function arg stats) =
  joinSpace
    [ joinEmpty [ bracket.roundOpen, text arg, bracket.roundClose ]
    , bracket.curlyOpen
    ]
    <> indent (line <> (intercalate line (map stat stats)))
    <> line
    <> bracket.curlyClose

expr (Binary op a b) = joinSpace [ expr a, bin op, expr b ]

expr (Unary op x) = joinSpace [ un op, expr x ]

expr (Length x) = joinEmpty [ expr x, dot, word.length ]

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
                (\(Tuple k v) -> line <> joinSpace [ joinEmpty [ text (show k), colon ], expr v ])
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
