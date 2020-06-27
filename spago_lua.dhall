let conf = ./spago.dhall

in conf //
  { name = "lua"
  , dependencies = conf.dependencies #
    [ "node-fs"
    , "prettier-printer"
    ]
  , sources = conf.sources #
    [ "lua/**/*.purs"
    , "test/**/*.purs"
    ]
}