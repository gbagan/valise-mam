{ name = "purescript"
, dependencies =
  [ "argonaut-codecs"
  , "arrays"
  , "debug"
  , "effect"
  , "exists"
  , "free"
  , "js-timers"
  , "maybe"
  , "pha"
  , "prelude"
  , "profunctor-lenses"
  , "strings"
  , "typelevel-prelude"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
