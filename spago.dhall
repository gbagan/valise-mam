{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript"
, dependencies =
    [ "arrays"
    , "debug"
    , "effect"
    , "foreign"
    , "maybe"
    , "pha"
    , "prelude"
    , "profunctor-lenses"
    , "strings"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
