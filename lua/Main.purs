module Main where

import Prelude
import CodeGen.Lua (impToLua, mkModPath, modPathJoiner)
import CodeGen.Lua.Printer (print)
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
outDir = "outlua"

main :: Effect Unit
main = do
  dirs <- S.readdir baseDir
  let
    modDirs = delete "cache-db.json" dirs
  orMakeDir outDir
  for_ modDirs \modName -> do
    let
      jsonPath = joinWith "/" [ baseDir, modName, "corefn.json" ]

      replacedModName = replaceAll (Pattern ".") (Replacement modPathJoiner) modName

      luaPath = joinWith "/" [ outDir, replacedModName, "index.lua" ]
    isJson <- S.exists jsonPath
    isLua <- S.exists luaPath
    -- process only modules which has corefn
    when isJson do
      if isLua then do
        statJson <- S.stat jsonPath
        statLua <- S.stat luaPath
        let
          mtimeJson = St.modifiedTime statJson

          mtimeLua = St.modifiedTime statLua
        -- process only modules modified
        when (mtimeLua < mtimeJson) do
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

        lua = impToLua optMod

        modDir = joinWith "/" [ outDir, mkModPath impMod.moduleName ]
      orMakeDir modDir
      S.writeTextFile UTF8 (joinWith "/" [ modDir, "index.lua" ]) (print lua)
      log $ mkModPath impMod.moduleName

orMakeDir :: FilePath -> Effect Unit
orMakeDir dir = do
  isDir <- S.exists dir
  when (not isDir) $ S.mkdir dir
