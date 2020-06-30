# psimp

This is an attempt to extract PureScript's CoreImp AST for dynamically typed languages.

Transpile target languages:

- Lua
- Dart
- JavaScript

## Requirements

- spago
- purescript
- node ✅v14.4.0
- lua ✅v5.3.5
- dart ✅v2.8.4

## Code Generation

Generate [example](./example) modules' `corefn.json` files.

```
spago -x example.dhall build --purs-args "-g corefn"
```

### Lua

Generate Lua files to `./outlua`

```
spago -x spago_lua.dhall run
```

Copy FFI files (manually for now) from [pslua-ffi](https://github.com/opyapeus/pslua-ffi)

```
cp -r [clonedir]/pslua-ffi/effect/* ./outlua
cp -r [clonedir]/pslua-ffi/console/* ./outlua
cp -r [clonedir]/pslua-ffi/prelude/* ./outlua
```

Run

```
LUA_PATH=outlua/?.lua lua main.lua
```

### Dart

Generate Dart files to `./outdart`

```
spago -x spago_dart.dhall run
```

Copy FFI files (manually for now) from [psdart-ffi](https://github.com/opyapeus/psdart-ffi)

```
cp -r [clonedir]/psdart-ffi/effect/* ./outdart
cp -r [clonedir]/psdart-ffi/console/* ./outdart
cp -r [clonedir]/psdart-ffi/prelude/* ./outdart
```

Run

```
dart main.dart
```

### JavaScript

Generate JavaScript files to `./outjs` (FFI files copied from `./output`)

```
spago -x spago_js.dhall run
```

Run

```
node main.js
```
