let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200507/packages.dhall sha256:9c1e8951e721b79de1de551f31ecb5a339e82bbd43300eb5ccfb1bf8cf7bbd62

let overrides = {=}

let additions =
  { corefn =
      { dependencies =
          [ "foreign-generic"
          , "profunctor"
          ]
      , repo =
          "https://github.com/opyapeus/purescript-corefn"
      , version =
          "ad6014"
      }
  , prettier-printer =
      { dependencies = [] : List Text 
      , repo =
          "https://github.com/opyapeus/purescript-prettier-printer"
      , version =
          "711de2"
      }
  }

in  upstream // overrides // additions
