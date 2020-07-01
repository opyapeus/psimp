module CodeGen.Lua.Common where

import Prelude
import CoreFn.Ident (Ident(..)) as CF
import CoreFn.Names (ProperName(..)) as CF
import Data.Foldable (elem)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import PSString (PSString(..))

-- | Convert a 'ProperName' into a valid Lua identifier
properToLua :: CF.ProperName -> String
properToLua (CF.ProperName s) = anyNameToLua s

-- | Convert a 'Ident' into a valid Lua identifier
identToLua :: CF.Ident -> String
identToLua (CF.Ident s) = anyNameToLua s

identToLua (CF.GenIdent _ _) = "GenIdent"

identToLua CF.UnusedIdent = "UnusedIdent"

-- | Convert a 'PSString' into a valid Lua identifier
psstringToLua :: PSString -> String
psstringToLua (PSString s) = anyNameToLua s

anyNameToLua :: String -> String
anyNameToLua s
  | elem s reserved = "_" <> s
  | otherwise =
    replaceAll (Pattern "$") (Replacement "_")
      <<< replaceAll (Pattern "'") (Replacement "_")
      $ s

reserved :: Array String
reserved =
  [ "and"
  , "break"
  , "do"
  , "else"
  , "elseif"
  , "end"
  , "false"
  , "for"
  , "function"
  , "goto"
  , "if"
  , "in"
  , "local"
  , "nil"
  , "not"
  , "or"
  , "repeat"
  , "return"
  , "then"
  , "true"
  , "until"
  , "while"
  ]
