module CoreImp.Constant where

import CoreFn.Names as CF

primModules :: Array CF.ModuleName
primModules =
  [ prim
  , primBoolean
  , primCoerce
  , primOrdering
  , primRow
  , primRowList
  , primSymbol
  , primTypeError
  ]

prim :: CF.ModuleName
prim = CF.ModuleName [ CF.ProperName "Prim" ]

primBoolean :: CF.ModuleName
primBoolean = CF.ModuleName [ CF.ProperName "Prim", CF.ProperName "Boolean" ]

primCoerce :: CF.ModuleName
primCoerce = CF.ModuleName [ CF.ProperName "Prim", CF.ProperName "Coerce" ]

primOrdering :: CF.ModuleName
primOrdering = CF.ModuleName [ CF.ProperName "Prim", CF.ProperName "Ordering" ]

primRow :: CF.ModuleName
primRow = CF.ModuleName [ CF.ProperName "Prim", CF.ProperName "Row" ]

primRowList :: CF.ModuleName
primRowList = CF.ModuleName [ CF.ProperName "Prim", CF.ProperName "RowList" ]

primSymbol :: CF.ModuleName
primSymbol = CF.ModuleName [ CF.ProperName "Prim", CF.ProperName "Symbol" ]

primTypeError :: CF.ModuleName
primTypeError = CF.ModuleName [ CF.ProperName "Prim", CF.ProperName "TypeError" ]
