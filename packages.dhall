let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220429/packages.dhall
        sha256:03c682bff56fc8f9d8c495ffcc6f524cbd3c89fe04778f965265c08757de8c9d

let additions =
      { pha =
        { dependencies =
          [ "aff", "effect", "free", "web-uievents", "unsafe-reference" ]
        , repo = "https://github.com/gbagan/purescript-pha.git"
        , version = "master"
        }
      } //
      { web-pointerevents = 
        { dependencies = ["web-uievents"]
        , repo = "https://github.com/gbagan/purescript-web-pointerevents.git"
        , version = "main"
        }
      }

in  upstream // additions
