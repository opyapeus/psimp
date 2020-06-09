{ name = "pslua"
, dependencies =
  [ "corefn"
  , "node-fs"
  , "prettier-printer"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
