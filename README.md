# pslua

generate corefn of example modules
```
spago -x example.dhall build --purs-args "-g corefn"
```

generate lua modules
```
spago run
```

copy ffi files manually
```
cp -r [pslua-ffi]/effect/* ./outlua
cp -r [pslua-ffi]/console/* ./outlua
cp -r [pslua-ffi]/prelude/* ./outlua
```

run
```
LUA_PATH=outlua/?.lua lua main.lua
```

all except ffi
```
spago -x example.dhall build --purs-args "-g corefn" && spago run && LUA_PATH=outlua/?.lua lua main.lua
```
