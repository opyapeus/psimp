module CodeGen.Dart.Common where

import Prelude
import CoreFn.Ident (Ident(..)) as CF
import CoreFn.Names (ProperName(..)) as CF
import Data.Foldable (elem)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import PSString (PSString(..))

public :: CF.Ident -> String
public = append "pub__" <<< identToDart

private :: CF.Ident -> String
private = append "_pvt_" <<< identToDart

-- | Convert a 'ProperName' into a valid Dart identifier
properToDart :: CF.ProperName -> String
properToDart (CF.ProperName s) = anyNameToDart s

-- | Convert a 'Ident' into a valid Dart identifier
identToDart :: CF.Ident -> String
identToDart (CF.Ident s) = anyNameToDart s

identToDart (CF.GenIdent _ _) = "GenIdent"

identToDart CF.UnusedIdent = "UnusedIdent"

-- | Convert a 'PSString' into a valid Dart identifier
psstringToDart :: PSString -> String
psstringToDart (PSString s) = anyNameToDart s

anyNameToDart :: String -> String
anyNameToDart s
  | elem s reserved = "_" <> s
  | otherwise = replaceAll (Pattern "'") (Replacement "_") s

reserved :: Array String
reserved =
  [ "abstract"
  , "else"
  , "import"
  , "super"
  , "as"
  , "enum"
  , "in"
  , "switch"
  , "assert"
  , "export"
  , "interface"
  , "sync "
  , "async"
  , "extends"
  , "is"
  , "this"
  , "await"
  , "extension"
  , "library"
  , "throw"
  , "break"
  , "external"
  , "mixin"
  , "true"
  , "case"
  , "factory"
  , "new"
  , "try"
  , "catch"
  , "false"
  , "null"
  , "typedef "
  , "class"
  , "final"
  , "on"
  , "var"
  , "const"
  , "finally"
  , "operator"
  , "void"
  , "continue"
  , "for"
  , "part"
  , "while"
  , "covariant"
  , "Function"
  , "rethrow"
  , "with"
  , "default"
  , "get"
  , "return"
  , "yield "
  , "deferred"
  , "hide"
  , "set"
  , "do"
  , "if"
  , "show"
  , "dynamic"
  , "implements"
  , "static"
  ]
