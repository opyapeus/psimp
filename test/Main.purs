module Test.Main where

import Prelude
import CodeGen.JS (impToJS)
import CodeGen.JS.Printer (print)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExcept)
import CoreFn.Ann (Ann)
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
  -- to show module data except annotation
  case runExcept (moduleFromJSON json) of
    Left err -> log $ show err
    Right mod -> do
      log $ show (mod.module :: Module EmptyAnn)
      log ""
  case runExcept (moduleFromJSON json) of
    Left err -> log $ show err
    Right mod -> do
      res <- try $ fnToImp (mod.module :: Module Ann)
      case res of
        Left err -> log $ show err
        Right impMod -> do
          let
            optMod = impMod { moduleStats = map optimize impMod.moduleStats }

            js = impToJS optMod
          log $ show impMod
          log ""
          log $ show impMod.moduleStats
          log ""
          log $ show optMod.moduleStats
          log ""
          log $ print js
          log ""
  log "🍝"
