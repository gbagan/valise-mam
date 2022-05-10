module Game.Bicolor.View where

import MamPrelude

import Game.Core (_position, _nbRows, _nbColumns)
import Game.Bicolor.Model (State, Msg(..), Card(..), Phase(..), Mode(..), _mode, _phase)
import Lib.Util (coords)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSelectGroup', iundo, iredo, ireset, irules)
import UI.Template (template, card, incDecGrid, gridStyle)

square ∷ ∀a. Card → Array (H.Prop a) → Html a
square card props = 
    H.div ([H.class_ "bicolor-square"] <> props)
    [   H.div [H.class_ "bicolor-square-inner", H.class' "blanc" (card == WhiteCard),
                H.class' "empty" (card == EmptyCard)]
        [   H.div [H.class_ "bicolor-square-blanc"] 
            [    H.svg [H.class_ "ui-absolute bicolor-card"] [H.use [P.href "#card"]]
            ]
        ,   H.div [H.class_ "bicolor-square-noir"] []
        ]
    ]

view ∷ State → Html Msg
view state = template {config, board, rules, winTitle} state where
    rows = state ^. _nbRows
    columns = state ^. _nbColumns
    position = state ^. _position

    icustom = iconbutton
                state
                {icon: IconSymbol "#customize", selected: state^._phase == PrepPhase}
                [E.onClick \_ → ToggleCustom]

    ishuffle = iconbutton
                state
                {icon: IconSymbol "#shuffle", disabled: state^._phase == GamePhase}
                [E.onClick \_ → Shuffle]


    config = card "??????"
        [   iconSelectGroup' state "Type de la grille" (state^._mode) SetMode
            [   StandardMode ∧ _{icon = IconSymbol "#grid-normal", tooltip = Just "Normale"}
            ,   CylinderMode ∧ _{icon = IconSymbol "#grid-cylinder", tooltip = Just "Cylindrique"}
            ,   TorusMode ∧ _{icon = IconSymbol "#grid-torus", tooltip = Just "Torique"}
            ]
        ,   icongroup "Configuration" [icustom, ishuffle]
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> (_ $ state)
        ]

    grid = H.div ([H.class_ "ui-board"] <> gridStyle rows columns 4) $
        position # mapWithIndex \index card →
            let {row, col} = coords columns index in
            square card
            [   H.style "height" $ pc (0.86 / toNumber rows)
            ,   H.style "width" $ pc (0.68 / toNumber columns)
            ,   H.style "left" $ pc ((toNumber col + 0.16) / toNumber columns)
            ,   H.style "top" $ pc ((toNumber row + 0.07) / toNumber rows)
            ,   E.onClick \_ → if state^._phase == PrepPhase then ToggleCard index else Play index
            ]

    board = incDecGrid state [grid]

    winTitle = if all (_ == EmptyCard) position then "GAGNÉ" else "PERDU"

    rules =
        [   H.text "Le but du jeu est de retirer toutes les cartes."
        ,   H.br
        ,   H.text "Pour retirer une carte, elle doit être face blanche, et alors on retourne les cartes adjacentes."
        ]
