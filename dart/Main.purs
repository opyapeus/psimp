module Main where

import Prelude
import CodeGen.Dart (impToDart, mkModPath)
import CodeGen.Dart.Optimizer (optimize)
import CodeGen.Dart.Printer (print)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExcept)
import CoreFn.Ann (Ann)
import CoreFn.FromJSON (moduleFromJSON)
import CoreFn.Module (Module)
import CoreImp.Desugar (fnToImp)
import CoreImp.Module (optimizeModule)
import Data.Array (delete)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Class.Console (info, log)
import Effect.Console (error)
import Node.Encoding (Encoding(..))
import Node.FS.Stats as St
import Node.FS.Sync as S
import Node.Path (FilePath)

baseDir :: FilePath
baseDir = "output"

outDir :: FilePath
outDir = "outdart"

main :: Effect Unit
main = do
  dirs <- S.readdir baseDir
  let
    modDirs = delete "cache-db.json" dirs
  orMakeDir outDir
  for_ modDirs \modName -> do
    let
      jsonPath = joinWith "/" [ baseDir, modName, "corefn.json" ]

      foreignPath = joinWith "/" [ baseDir, modName, "foreign.dart" ]

      dartPath = joinWith "/" [ outDir, modName, "index.dart" ]
    isJson <- S.exists jsonPath
    isForeign <- S.exists foreignPath
    isDart <- S.exists dartPath
    -- process only modules which has corefn
    when isJson do
      if isDart then do
        statJson <- S.stat jsonPath
        statDart <- S.stat dartPath
        let
          mtimeJson = St.modifiedTime statJson

          mtimeDart = St.modifiedTime statDart
        -- process only modules modified
        when (mtimeDart < mtimeJson) do
          processJson jsonPath
      else do
        processJson jsonPath
        -- copy foreign.dart
        when isForeign do
          dart <- S.readTextFile UTF8 foreignPath
          S.writeTextFile UTF8 (joinWith "/" [ outDir, modName, "foreign.dart" ]) dart
          log "copied foregin.dart"
  info "--- transpiled! ---"

processJson :: FilePath -> Effect Unit
processJson jsonPath = do
  json <- S.readTextFile UTF8 jsonPath
  case runExcept (moduleFromJSON json) of
    Left err -> error $ show err
    Right mod -> transpile mod.module

transpile :: Module Ann -> Effect Unit
transpile mod = do
  res <- try $ fnToImp mod
  case res of
    Left err -> error $ show err
    Right impMod -> do
      let
        optMod = optimizeModule impMod

        dart = impToDart optMod

        optDart = map optimize dart

        modDir = joinWith "/" [ outDir, mkModPath impMod.moduleName ]
      orMakeDir modDir
      S.writeTextFile UTF8 (joinWith "/" [ modDir, "index.dart" ]) (print optDart)
      log $ mkModPath impMod.moduleName

orMakeDir :: FilePath -> Effect Unit
orMakeDir dir = do
  isDir <- S.exists dir
  when (not isDir) $ S.mkdir dir
