# pslua

A purescript transpiler for lua language.

※ Not well tested.

## Requirements

- spago
- purescript
- node
- lua

## Code Generation

generate `corefn.json` of example modules

```
spago -x example.dhall build --purs-args "-g corefn"
```

generate lua files to `./outlua`

```
spago -x spago_lua.dhall run
```

copy ffi files (manually for now) from [pslua-ffi](https://github.com/opyapeus/pslua-ffi)

```
cp -r [somedir]/pslua-ffi/effect/* ./outlua
cp -r [somedir]/pslua-ffi/console/* ./outlua
cp -r [somedir]/pslua-ffi/prelude/* ./outlua
```

run

```
LUA_PATH=outlua/?.lua lua main.lua
```
