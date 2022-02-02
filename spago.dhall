{ name = "boxes"
, dependencies =
  [ "prelude"
  , "psci-support"
  , "stringutils"
  , "arrays"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "profunctor"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/cdepillabout/purescript-boxes"
}
