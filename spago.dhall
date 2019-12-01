{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript"
, dependencies =
    [ "affjax"
    , "argonaut"
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
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
