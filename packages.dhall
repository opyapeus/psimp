let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8/packages.dhall sha256:0e95ec11604dc8afc1b129c4d405dcc17290ce56d7d0665a0ff15617e32bbf03
      with coreimp = ./coreimp/spago.dhall as Location

let overrides = {=}

let additions =
      { corefn =
        { dependencies = [ "foreign-generic", "profunctor" ]
        , repo = "https://github.com/opyapeus/purescript-corefn"
        , version = "ad6014"
        }
      , prettier-printer =
        { dependencies = [] : List Text
        , repo = "https://github.com/opyapeus/purescript-prettier-printer"
        , version = "711de2"
        }
      }

in  upstream // overrides // additions
