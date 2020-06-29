let conf = ./spago.dhall

in conf //
  { name = "dart"
  , dependencies = conf.dependencies #
    [ "node-fs"
    , "prettier-printer"
    ]
  , sources = conf.sources #
    [ "dart/**/*.purs"
    ]
}