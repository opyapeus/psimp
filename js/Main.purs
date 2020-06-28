module Main where

import Prelude
import CodeGen.JS (impToJS, mkModPath)
import CodeGen.JS.Printer (print)
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
outDir = "outjs"

main :: Effect Unit
main = do
  dirs <- S.readdir baseDir
  let
    modDirs = delete "cache-db.json" dirs
  orMakeDir outDir
  for_ modDirs \modName -> do
    let
      jsonPath = joinWith "/" [ baseDir, modName, "corefn.json" ]

      foreignPath = joinWith "/" [ baseDir, modName, "foreign.js" ]

      jsPath = joinWith "/" [ outDir, modName, "index.js" ]
    isJson <- S.exists jsonPath
    isForeign <- S.exists foreignPath
    isJS <- S.exists jsPath
    -- process only modules which has corefn
    when isJson do
      if isJS then do
        statJson <- S.stat jsonPath
        statJS <- S.stat jsPath
        let
          mtimeJson = St.modifiedTime statJson

          mtimeJS = St.modifiedTime statJS
        -- process only modules modified
        when (mtimeJS < mtimeJson) do
          processJson jsonPath
      else do
        processJson jsonPath
        -- copy foreign.js
        when isForeign do
          js <- S.readTextFile UTF8 foreignPath
          S.writeTextFile UTF8 (joinWith "/" [ outDir, modName, "foreign.js" ]) js
          log "copied foregin.js"
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

        js = impToJS optMod

        modDir = joinWith "/" [ outDir, mkModPath impMod.moduleName ]
      orMakeDir modDir
      S.writeTextFile UTF8 (joinWith "/" [ modDir, "index.js" ]) (print js)
      log $ mkModPath impMod.moduleName

orMakeDir :: FilePath -> Effect Unit
orMakeDir dir = do
  isDir <- S.exists dir
  when (not isDir) $ S.mkdir dir
