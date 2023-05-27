module Game.Bicolor.View where

import MamPrelude

import Game.Core (_position, _nbRows, _nbColumns)
import Game.Bicolor.Model (Model, Msg(..), Card(..), Phase(..), Mode(..), _mode, _phase)
import Lib.Helpers (coords)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc)
import Pha.Svg as S
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSelectGroup', iundo, iredo, ireset, irules)
import UI.Template (template, card, incDecGrid, gridStyle)

square ∷ ∀ a. Card → Array (H.Prop a) → Html a
square card props =
  H.div ([ H.class_ "bicolor-square" ] <> props)
    [ H.div [ H.class_ "bicolor-square-inner", H.class' "blanc" (card == WhiteCard), H.class' "empty" (card == EmptyCard) ]
        [ H.div [ H.class_ "bicolor-square-blanc" ]
            [ S.svg [ H.class_ "ui-absolute bicolor-card" ] [ S.use [ P.href "#card" ] ]
            ]
        , H.div [ H.class_ "bicolor-square-noir" ] []
        ]
    ]

view ∷ Model → Html Msg
view model = template { config, board, rules, winTitle } model
  where
  rows = model ^. _nbRows
  columns = model ^. _nbColumns
  position = model ^. _position

  icustom = iconbutton
    model
    { icon: IconSymbol "#customize", selected: model ^. _phase == PrepPhase }
    [ E.onClick \_ → ToggleCustom ]

  ishuffle = iconbutton
    model
    { icon: IconSymbol "#shuffle", disabled: model ^. _phase == GamePhase }
    [ E.onClick \_ → Shuffle ]

  config = card "??????"
    [ iconSelectGroup' model "Type de la grille" (model ^. _mode) SetMode
        [ StandardMode ∧ _ { icon = IconSymbol "#grid-normal", tooltip = Just "Normale" }
        , CylinderMode ∧ _ { icon = IconSymbol "#grid-cylinder", tooltip = Just "Cylindrique" }
        , TorusMode ∧ _ { icon = IconSymbol "#grid-torus", tooltip = Just "Torique" }
        ]
    , icongroup "Configuration" [ icustom, ishuffle ]
    , icongroup "Options" $ [ iundo, iredo, ireset, irules ] <#> (_ $ model)
    ]

  grid = H.div ([ H.class_ "ui-board" ] <> gridStyle rows columns 4)
    $ position
    # mapWithIndex \index card →
        let
          { row, col } = coords columns index
        in
          square card
            [ H.style "height" $ pc (0.86 / toNumber rows)
            , H.style "width" $ pc (0.68 / toNumber columns)
            , H.style "left" $ pc ((toNumber col + 0.16) / toNumber columns)
            , H.style "top" $ pc ((toNumber row + 0.07) / toNumber rows)
            , E.onClick \_ → if model ^. _phase == PrepPhase then ToggleCard index else Play index
            ]

  board = incDecGrid model [ grid ]

  winTitle = if all (_ == EmptyCard) position then "GAGNÉ" else "PERDU"

  rules =
    [ H.text "Le but du jeu est de retirer toutes les cartes."
    , H.br
    , H.text "Pour retirer une carte, elle doit être face blanche, et alors on retourne les cartes adjacentes."
    ]
