let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0/packages.dhall sha256:710b53c085a18aa1263474659daa0ae15b7a4f453158c4f60ab448a6b3ed494e

let overrides = {=}

let additions =
      { pha =
        { dependencies =
          [ "aff", "effect", "free", "web-uievents", "unsafe-reference" ]
        , repo = "https://github.com/gbagan/purescript-pha.git"
        , version = "v0.8.1"
        }
      }

in  upstream // overrides // additions
