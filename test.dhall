{ name = "tests"
, dependencies = (./spago.dhall).dependencies #
  [ "spec"
  , "aff"
  , "effect"
  ]
, packages = (./spago.dhall).packages
, sources = (./spago.dhall).sources # [ "test/**/*.purs" ]
}
