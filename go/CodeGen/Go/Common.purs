module CodeGen.Go.Common where

import Prelude
import CoreFn.Ident (Ident(..)) as CF
import CoreFn.Names (ProperName(..)) as CF
import Data.Foldable (elem)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import PSString (PSString(..))

public :: CF.Ident -> String
public = append "Pub_" <<< identToGo

private :: CF.Ident -> String
private = append "pvt_" <<< identToGo

-- | Convert a 'ProperName' into a valid Go identifier
properToGo :: CF.ProperName -> String
properToGo (CF.ProperName s) = anyNameToGo s

-- | Convert a 'Ident' into a valid Go identifier
identToGo :: CF.Ident -> String
identToGo (CF.Ident s) = anyNameToGo s

identToGo (CF.GenIdent ms i) = "GenIdent"

identToGo CF.UnusedIdent = "UnusedIdent"

-- | Convert a 'PSString' into a valid Go identifier
psstringToGo :: PSString -> String
psstringToGo (PSString s) = anyNameToGo s

anyNameToGo :: String -> String
anyNameToGo s
  | elem s reserved = "_" <> s
  | otherwise =
    replaceAll (Pattern "$") (Replacement "_")
      <<< replaceAll (Pattern "'") (Replacement "_")
      $ s

reserved :: Array String
reserved =
  [ "break"
  , "default"
  , "func"
  , "interface"
  , "select"
  , "case"
  , "defer"
  , "go"
  , "map"
  , "struct"
  , "chan"
  , "else"
  , "goto"
  , "package"
  , "switch"
  , "const"
  , "fallthrough"
  , "if"
  , "range"
  , "type"
  , "continue"
  , "for"
  , "import"
  , "return"
  , "var"
  ]
