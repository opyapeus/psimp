# psimp

This is an attempt to extract PureScript's CoreImp AST for dynamically typed imperative languages.

It is based on a past [coreimp work](https://github.com/purescript/purescript/tree/core-imp/src/Language/PureScript/CoreImp)

The design concept is to provide a simpler [AST](./coreimp/src/CoreImp/AST.purs) rather than performance considerations.

Transpile target languages:

- JavaScript
- Lua
- Dart

## Requirements

- yarn
- node ✅v14.4.0
- lua ✅v5.3.5
- dart ✅v2.8.4

### Requirements with Nix

A simple way to get the requirements in Nix is to use a one-liner nix-shell command.
For instance, if you are just in lua but not dart, you could do:

```
nix-shell -p lua yarn nodejs
```


## Code Generation

First, generate [example](./example/src) modules' `corefn.json` files to `./example/output`

```
yarn example
```

### JavaScript

Generate JavaScript files to `./js/outjs` (FFI files copied from `./example/output`)

```
yarn js:codegen
```

Run

```
yarn js:execute
```

### Lua

Generate Lua files to `./lua/outlua`

```
yarn lua:codegen
```

Copy FFI files (manually for now) from [pslua-ffi](https://github.com/opyapeus/pslua-ffi)

```
cp -r [clonedir]/pslua-ffi/effect/* ./lua/outlua
cp -r [clonedir]/pslua-ffi/console/* ./lua/outlua
cp -r [clonedir]/pslua-ffi/prelude/* ./lua/outlua
```

Run

```
yarn lua:execute
```

### Dart

Generate Dart files to `./dart/outdart`

```
yarn dart:codegen
```

Copy FFI files (manually for now) from [psdart-ffi](https://github.com/opyapeus/psdart-ffi)

```
cp -r [clonedir]/psdart-ffi/effect/* ./dart/outdart
cp -r [clonedir]/psdart-ffi/console/* ./dart/outdart
cp -r [clonedir]/psdart-ffi/prelude/* ./dart/outdart
```

Run

```
yarn dart:execute
```

## Memo

It is not cut out as a library for now, but is a monorepo to modify and check the model easily.
