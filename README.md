# pslua

A purescript transpiler for lua language.

※ Not well tested.

## Requirements

- spago
- purescript
- node
- lua
- dart

## Code Generation

generate `corefn.json` of example modules

```
spago -x example.dhall build --purs-args "-g corefn"
```

### Lua

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

### Dart

generate lua files to `./outdart`

```
spago -x spago_dart.dhall run
```

copy ffi files (manually for now) from [psdart-ffi](https://github.com/opyapeus/psdart-ffi)

```
cp -r [somedir]/psdart-ffi/effect/* ./outdart
cp -r [somedir]/psdart-ffi/console/* ./outdart
cp -r [somedir]/psdart-ffi/prelude/* ./outdart
```

run

```
dart main.dart
```

### JavaScript

generate lua files to `./outjs`

```
spago -x spago_js.dhall run
```

run (ffi files copied from `./output`)

```
node main.js
```
