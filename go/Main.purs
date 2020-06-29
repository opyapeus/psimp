module Main where

import Prelude
import CodeGen.Go (impToGo, mkModName)
import CodeGen.Go.Optimizer (optimize)
import CodeGen.Go.Printer (print)
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
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll)
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
outDir = "outgo"

main :: Effect Unit
main = do
  dirs <- S.readdir baseDir
  let
    modDirs = delete "cache-db.json" dirs
  orMakeDir outDir
  for_ modDirs \modName -> do
    let
      jsonPath = joinWith "/" [ baseDir, modName, "corefn.json" ]

      replacedModName = replaceAll (Pattern ".") (Replacement "_") modName

      goPath = joinWith "/" [ outDir, replacedModName, "index.go" ]
    isJson <- S.exists jsonPath
    isGo <- S.exists goPath
    -- process only modules which has corefn
    when isJson do
      if isGo then do
        statJson <- S.stat jsonPath
        statGo <- S.stat goPath
        let
          mtimeJson = St.modifiedTime statJson

          mtimeGo = St.modifiedTime statGo
        -- process only modules modified
        when (mtimeGo < mtimeJson) do
          processJson jsonPath
      else do
        processJson jsonPath
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

        go = impToGo optMod

        optGo = map optimize go

        modDir = joinWith "/" [ outDir, mkModName impMod.moduleName ]
      orMakeDir modDir
      S.writeTextFile UTF8 (joinWith "/" [ modDir, "index.go" ]) (print optGo)
      log $ mkModName impMod.moduleName

orMakeDir :: FilePath -> Effect Unit
orMakeDir dir = do
  isDir <- S.exists dir
  when (not isDir) $ S.mkdir dir
