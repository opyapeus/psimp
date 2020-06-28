let conf = ./spago.dhall

in conf //
  { name = "js"
  , dependencies = conf.dependencies #
    [ "node-fs"
    , "prettier-printer"
    ]
  , sources = conf.sources #
    [ "js/**/*.purs"
    ]
}