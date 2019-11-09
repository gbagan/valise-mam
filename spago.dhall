{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "purescript"
, dependencies =
    [
      "arrays"
    , "effect"
    , "maybe"
    , "prelude"
    , "profunctor-lenses"
    , "run"
    , "strings"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
