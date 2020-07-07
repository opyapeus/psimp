module CodeGen.JS.Common where

import Prelude
import CoreFn.Ident (Ident(..)) as CF
import CoreFn.Names (ProperName(..)) as CF
import Data.Foldable (elem)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import PSString (PSString(..))

-- | Convert a 'ProperName' into a valid JS identifier
properToJS :: CF.ProperName -> String
properToJS (CF.ProperName s) = anyNameToJS s

-- | NOTE: use direct name to refer raw foreign.js (ex. exports["uncons'"])
foreignIdentToJS :: CF.Ident -> String
foreignIdentToJS (CF.Ident s) = s

foreignIdentToJS i = identToJS i

-- | Convert a 'Ident' into a valid JS identifier
identToJS :: CF.Ident -> String
identToJS (CF.Ident s) = anyNameToJS s

identToJS (CF.GenIdent _ _) = "GenIdent"

identToJS CF.UnusedIdent = "UnusedIdent"

-- | Convert a 'PSString' into a valid JS identifier
psstringToJS :: PSString -> String
psstringToJS (PSString s) = anyNameToJS s

anyNameToJS :: String -> String
anyNameToJS s
  | elem s reserved = "_" <> s
  | otherwise = replaceAll (Pattern "'") (Replacement "_") s

reserved :: Array String
reserved =
  [ "abstract"
  , "arguments"
  , "await"
  , "boolean"
  , "break"
  , "byte"
  , "case"
  , "catch"
  , "char"
  , "class"
  , "const"
  , "continue"
  , "debugger"
  , "default"
  , "delete"
  , "do"
  , "double"
  , "else"
  , "enum"
  , "eval"
  , "export"
  , "extends"
  , "false"
  , "final"
  , "finally"
  , "float"
  , "for"
  , "function"
  , "goto"
  , "if"
  , "implements"
  , "import"
  , "in"
  , "instanceof"
  , "int"
  , "interface"
  , "let"
  , "long"
  , "native"
  , "new"
  , "null"
  , "package"
  , "private"
  , "protected"
  , "public"
  , "return"
  , "short"
  , "static"
  , "super"
  , "switch"
  , "synchronized"
  , "this"
  , "throw"
  , "throws"
  , "transient"
  , "true"
  , "try"
  , "typeof"
  , "var"
  , "void"
  , "volatile"
  , "while"
  , "with"
  , "yield"
  ]
