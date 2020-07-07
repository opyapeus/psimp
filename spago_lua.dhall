let conf = ./spago.dhall

in conf //
  { name = "lua"
  , dependencies = conf.dependencies #
    [ "node-fs"
    , "prettier-printer"
    , "console"
    ]
  , sources = conf.sources #
    [ "lua/**/*.purs"
    ]
}