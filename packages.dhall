let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221229/packages.dhall
        sha256:a6af1091425f806ec0da34934bb6c0ab0ac1598620bbcbb60a7d463354e7d87c

let additions =
      { pha =
        { dependencies =
          [ "aff"
          , "effect"
          , "free"
          , "web-uievents"
          , "unsafe-reference"
          , "web-pointerevents"
          ]
        , repo = "https://github.com/gbagan/purescript-pha.git"
        , version = "master"
        }
      }

in  upstream // additions
