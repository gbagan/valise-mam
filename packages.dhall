let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.3-20210722/packages.dhall sha256:1ceb43aa59436bf5601bac45f6f3781c4e1f0e4c2b8458105b018e5ed8c30f8c

let overrides = {=}

let additions =
      { pha =
        { dependencies =
          [ "aff", "effect", "free", "web-uievents", "unsafe-reference" ]
        , repo = "https://github.com/gbagan/purescript-pha.git"
        , version = "master"
        }
      }

in  upstream // overrides // additions
