{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript"
, dependencies =
  [ "argonaut-codecs"
  , "arrays"
  , "debug"
  , "effect"
  , "foreign"
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
