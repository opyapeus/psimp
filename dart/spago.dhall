{ name = "dart"
, dependencies =
  [ "coreimp"
  , "node-fs"
  , "prettier-printer"
  , "console"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs" ]
}
