{ name = "purescript-formula"
, dependencies =
  [ "dom-simple"
  , "effect"
  , "prelude"
  , "reactix"
  , "record"
  , "toestand"
  , "typisch"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
