module Test.Main where

import Prelude
import CodeGen.Lua (impToLua)
import CodeGen.Lua.Printer (print)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExcept)
import CoreFn.FromJSON (moduleFromJSON)
import CoreFn.Module (Module)
import CoreImp.Desugar (fnToImp)
import CoreImp.Optimizer (optimize)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import EmptyAnn (EmptyAnn)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as S

main :: Effect Unit
main = do
  json <- S.readTextFile UTF8 "output/Sample/corefn.json"
  case runExcept (moduleFromJSON json) of
    Left err -> log $ show err
    Right mod -> do
      log $ show mod
      log ""
      res <- try $ fnToImp (mod.module :: Module EmptyAnn)
      case res of
        Left err -> log $ show err
        Right impMod -> do
          let
            optMod = impMod { moduleStats = map optimize impMod.moduleStats }

            lua = impToLua optMod
          log $ show impMod
          log ""
          log $ show impMod.moduleStats
          log ""
          log $ show optMod.moduleStats
          log ""
          log $ print lua
          log ""
  log "🍝"
