let conf = ./spago.dhall

in conf //
  { name = "go"
  , dependencies = conf.dependencies #
    [ "node-fs"
    , "prettier-printer"
    ]
  , sources = conf.sources #
    [ "go/**/*.purs"
  ]
}