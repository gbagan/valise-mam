let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201206/packages.dhall sha256:c9ffd7577fb8ee2197309591d7ccc0f506ee37b9078866f0ef159f5abbb1b32b

let overrides = {=}

let additions =
      { pha =
        { dependencies = ["aff", "effect", "free", "web-uievents", "unsafe-reference"]
        , repo = "https://github.com/gbagan/purescript-pha.git"
        , version = "v0.7.3"
        }
      }

in  upstream // overrides // additions
