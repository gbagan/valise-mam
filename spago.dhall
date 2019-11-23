{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript"
, dependencies =
    [ "arrays", "debug", "effect", "maybe", "pha", "prelude", "strings", "profunctor-lenses" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
