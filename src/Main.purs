module Main where

import Prelude
import CodeGen.Lua (impToLua, mkModName)
import CodeGen.Lua.Printer (print)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExcept)
import CoreFn.FromJSON (moduleFromJSON)
import CoreFn.Module (Module)
import CoreImp.Desugar (fnToImp)
import CoreImp.Optimizer (optimize)
import Data.Array (delete)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll)
import Effect (Effect)
import Effect.Class.Console (info, log)
import Effect.Console (error)
import EmptyAnn (EmptyAnn)
import Node.Encoding (Encoding(..))
import Node.FS.Stats as St
import Node.FS.Sync as S

baseDir :: String
baseDir = "output"

outDir :: String
outDir = "outlua"

main :: Effect Unit
main = do
  isOutDir <- S.exists outDir
  when (not isOutDir) $ S.mkdir outDir
  dirs <- S.readdir baseDir
  let
    dirs' = delete "cache-db.json" dirs
  for_ dirs' \modName -> do
    isJson <- S.exists (jsonPath modName)
    isLua <- S.exists (luaPath modName)
    -- process only modules which has corefn
    when isJson do
      if isLua then do
        statJson <- S.stat (jsonPath modName)
        statLua <- S.stat (luaPath modName)
        let
          mtimeJson = St.modifiedTime statJson

          mtimeLua = St.modifiedTime statLua
        -- process only modules modified
        when (mtimeLua < mtimeJson) do
          processJson modName
      else do
        processJson modName
  info "--- transpiled! ---"

processJson :: String -> Effect Unit
processJson modName = do
  json <- S.readTextFile UTF8 (jsonPath modName)
  case runExcept (moduleFromJSON json) of
    Left err -> error $ show err
    Right mod -> do
      transpile mod.module

transpile :: Module EmptyAnn -> Effect Unit
transpile mod = do
  res <- try $ fnToImp mod
  case res of
    Left err -> error $ show err
    Right impMod -> do
      let
        optMod = impMod { moduleStats = map optimize impMod.moduleStats }

        lua = impToLua optMod
      log $ mkModName impMod.moduleName
      let
        modDir = joinWith "/" [ outDir, mkModName impMod.moduleName ]
      isModDir <- S.exists modDir
      when (not isModDir) $ S.mkdir modDir
      S.writeTextFile UTF8 (joinWith "/" [ modDir, "index.lua" ]) (print lua)

jsonPath :: String -> String
jsonPath modName = joinWith "/" [ baseDir, modName, "corefn.json" ]

luaPath :: String -> String
luaPath modName = joinWith "/" [ outDir, modName', "index.lua" ]
  where
  modName' = replaceAll (Pattern ".") (Replacement "_") modName
