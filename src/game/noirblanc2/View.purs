module Game.Noirblanc2.View where

import MyPrelude

import Game.Core (_position, _nbRows, _nbColumns)
import Game.Noirblanc2.Model (State, Msg(..), Card(..), Phase(..), _phase)
import Lib.Util (coords)
import Pha as H
import Pha.Elements as HH
import Pha.Attributes as P
import Pha.Events as E
import Pha.Util (pc)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iundo, iredo, ireset, irules)
import UI.Template (template, card, incDecGrid, gridStyle)

square ∷ ∀a. Card → Array (H.Prop a) → H.VDom a
square card props = 
    HH.div ([H.class_ "noirblanc2-square"] <> props)
    [   HH.div [H.class_ "noirblanc2-square-inner", H.class' "blanc" (card == WhiteCard),
                H.class' "empty" (card == EmptyCard)]
        [   HH.div [H.class_ "noirblanc2-square-blanc"] []
        ,   HH.div [H.class_ "noirblanc2-square-noir"] []
        ]
    ]

view ∷ State → H.VDom Msg
view state = template {config, board, rules, winTitle} state where
    rows = state ^. _nbRows
    columns = state ^. _nbColumns
    position = state ^. _position

    icustom = iconbutton
                state
                {icon: IconSymbol "#customize", selected: state^._phase == PrepPhase}
                [E.onclick ToggleCustom]

    ishuffle = iconbutton
                state
                {icon: IconSymbol "#shuffle", disabled: state^._phase == GamePhase}
                [E.onclick Shuffle]


    config = card "Tout noir tout blanc"
        [   icongroup "Configuration" [icustom, ishuffle]
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> (_ $ state)
        ]

    grid = HH.div ([H.class_ "ui-board"] <> gridStyle rows columns 4) $
        position # mapWithIndex \index card →
            let {row, col} = coords columns index in
            square card
            [   H.key $ show index
            ,   H.style "height" $ pc (0.86 / toNumber rows)
            ,   H.style "width" $ pc (0.86 / toNumber columns)
            ,   H.style "left" $ pc ((toNumber col + 0.07) / toNumber columns)
            ,   H.style "top" $ pc ((toNumber row + 0.07) / toNumber rows)
            ,   E.onclick $ if state^._phase == PrepPhase then ToggleCard index else Play index
            ]

    board = incDecGrid state [grid]

    winTitle = if all (_ == EmptyCard) position then "GAGNE" else "PERDU"

    rules =
        [   H.text "Le but du jeu est de retourner des tuiles pour que toutes soient face noire."
        ,   HH.br
        ,   H.text "La difficulté est que lorsque tu retournes une tuile, les tuiles adjacentes sont également retournées."
        ,   HH.br
        ,   H.text "Ce jeu possède différents niveaux débloqués au fur et à mesure ainsi que d'autres modes de jeu. Selon le mode choisi, les règles pour retourner les tuiles changent."
        ]
