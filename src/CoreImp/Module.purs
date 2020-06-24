module CoreImp.Module where

import CoreFn.Ident (Ident) as CF
import CoreFn.Names (ModuleName) as CF
import CoreImp.AST as CI

type Module
  = { moduleName :: CF.ModuleName
    , moduleImports :: Array CF.ModuleName
    , moduleExports :: Array CF.Ident
    , moduleForeigns :: Array CF.Ident
    , moduleStats :: Array CI.Stat
    }
