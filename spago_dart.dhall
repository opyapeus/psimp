let conf = ./spago.dhall

in conf //
  { name = "dart"
  , dependencies = conf.dependencies #
    [ "node-fs"
    , "prettier-printer"
    , "console"
    ]
  , sources = conf.sources #
    [ "dart/**/*.purs"
    ]
}