{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "purescript"
, dependencies =
    [ "effect", "prelude", "maybe", "arrays", "strings", "profunctor-lenses", "aff"  ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
