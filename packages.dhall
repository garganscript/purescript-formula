let upstream =
      https://github.com/garganscript/package-sets/releases/download/v0.1.3/release.dhall
        sha256:23abbf7e1a02f15f8efe7fcfba8cd583011b4f80343b97d51d29833fb7f193cb

let overrides = {=}

let additions =
      { inflection =
        { dependencies = [ "functions" ]
        , repo = "https://github.com/athanclark/purescript-inflection"
        , version = "v1.0.0"
        }
      }

in  upstream // overrides // additions
