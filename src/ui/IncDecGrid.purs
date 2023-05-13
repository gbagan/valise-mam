module UI.IncDecGrid where

import MamPrelude
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Events as E
import UI.Icon (iconbutton, Icon(..))

type IncDecGridOptions msg =
  { locked ∷ Boolean
  , nbRows ∷ Int
  , nbColumns ∷ Int
  , customSize ∷ Boolean
  , showRowButtons ∷ Boolean
  , showColButtons ∷ Boolean
  , resize ∷ Int → Int → msg
  }

incDecGrid ∷ ∀ msg. IncDecGridOptions msg → Array (Html msg) → Html msg
incDecGrid { locked, nbRows, nbColumns, customSize, showRowButtons, showColButtons, resize } children =
  H.div [ H.class_ "ui-incdecgrid" ]
    [ H.div [ H.class_ "flex" ]
        [ H.div [ H.class_ "ui-flex-center ui-incdecgrid-container" ] children
        , H.when showRowButtons \_ →
            H.div [ H.class_ "ui-flex-center ui-incdecgrid-rows" ]
              [ iconbutton
                  { round: true
                  , icon: IconSymbol "#plus"
                  , disabled: locked
                  , hidden: not customSize
                  }
                  [ E.onClick $ \_ → resize (nbRows + 1) nbColumns ]
              , H.div [ H.class_ "ui-incdecgrid-text" ] [ H.span [] [ H.text $ show nbRows ] ]
              , iconbutton
                  { round: true
                  , icon: IconSymbol "#minus"
                  , disabled: locked
                  , hidden: not customSize
                  }
                  [ E.onClick $ \_ → resize (nbRows - 1) nbColumns ]
              ]
        ]
    , H.when showColButtons \_ →
        H.div [ H.class_ "ui-flex-center ui-incdecgrid-cols" ]
          [ iconbutton
              { round: true
              , icon: IconSymbol "#minus"
              , disabled: locked
              , hidden: not customSize
              }
              [ E.onClick $ \_ → resize nbRows (nbColumns - 1) ]
          , H.div [ H.class_ "ui-incdecgrid-text" ] [ H.span [] [ H.text $ show nbColumns ] ]
          , iconbutton
              { round: true
              , icon: IconSymbol "#plus"
              , disabled: locked
              , hidden: not customSize
              }
              [ E.onClick $ \_ → resize nbRows (nbColumns + 1) ]
          ]
    ]
