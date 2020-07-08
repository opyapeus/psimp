module CoreImp.Module where

import Prelude
import CoreFn.Ident (Ident) as CF
import CoreFn.Names (ModuleName) as CF
import CoreImp.AST (Stat)
import CoreImp.Optimizer (optimize)

type Module
  = { moduleName :: CF.ModuleName
    , moduleImports :: Array CF.ModuleName
    , moduleExports :: Array CF.Ident
    , moduleForeigns :: Array CF.Ident
    , moduleStats :: Array Stat
    }

optimizeModule :: Module -> Module
optimizeModule m = m { moduleStats = map optimize m.moduleStats }
