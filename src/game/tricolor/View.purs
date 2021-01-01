module Game.Tricolor.View where

import MyPrelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Game.Core (isLevelFinished, _position)
import Game.Tricolor.Model (State, Msg(..), _size, _nbColors, _range, _hoverCell, _shuffle, inRange)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Keyed as K
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (translate)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSelectGroup, iundo, iredo, ireset, irules)
import UI.Template (template, card)

colors ∷ Array String
colors = ["green", "yellow", "red", "magenta", "blue"]

translateCell ∷ Int → Int → String
translateCell i size = translate (show x) (show y) where
    x = 50.0 + 35.0 * cos (toNumber i * 2.0 * pi / toNumber size)
    y = 45.0 + 35.0 * sin (toNumber i * 2.0 * pi / toNumber size)

irandom ∷ State → Html Msg
irandom state =
    iconbutton
        state {icon: IconSymbol "#shuffle", tooltip: Just "Mélanger", selected: state^._shuffle}
        [   E.onclick Shuffle
        ]

view ∷ State → Html Msg
view state = template {config, board, rules} state where
    position = state ^. _position
    size = state ^. _size
    nbColors = state ^. _nbColors
    range = state ^. _range
    levelFinished = isLevelFinished state
    hoverCell = state ^. _hoverCell

    config =
        card "Feux tricolores" 
        [   iconSelectGroup state "Nombre de feux" [4, 5, 6, 7, 8, 9, 10, 11, 12, 13] size SetSize (const identity)
        ,   iconSelectGroup state "Nombre de couleurs" [2, 3, 4, 5] nbColors SetNbColors (const identity)
        ,   iconSelectGroup state "Portée" [1, 2, 3] range SetRange (const identity)
        ,   icongroup "Options" $ [ iundo, iredo, ireset, irandom, irules ] <#> (_ $ state)
        ]

    drawCell i color =
        ("b" <> show i) /\ H.circle 
        [   P.r 7.5
        ,   H.class_ "tricolor-cell"
        ,   H.class' "finished" levelFinished
        ,   P.stroke $ if (inRange state i <$> hoverCell) == Just true then "lightgreen" else "black"
        ,   P.fill $ if levelFinished then "" else colors !! color # fromMaybe ""
        ,   P.transform (translateCell i size)
        ,   E.onclick $ Play i
        ,   E.onpointerenter $ SetHoverCell (Just i)
        ,   E.onpointerleave $ SetHoverCell Nothing
        ]

    drawColorCycle =
        (take nbColors colors # foldMapWithIndex \i color →
                [   ("c" <> show i) /\ H.circle
                    [   P.cx $ toNumber (95 + 15 * (i - nbColors))
                    ,   P.cy 95.0
                    ,   P.r 3.0
                    ,   P.fill color
                    ]
                ,   ("t" <> show i) /\ H.path
                    [   P.d "M0 2H4V0l3 3l-3 3v-2h-4Z"
                    ,   P.fill "black"
                    ,   P.transform $ translate (show $ 99 + 15 * (i - nbColors)) "92"
                    ]
                ]
        ) <> ["fc" /\ H.circle [P.cx 95.0, P.cy 95.0, P.r 3.0, P.fill "green"]]

    board =
        H.div [H.class_ "ui-board tricolor-board"]
        [   K.svg [P.viewBox 0 0 100 100] $ concat
            [   position # mapWithIndex drawCell
            ,   drawColorCycle
            ]
        ]

    rules = 
        [   H.text "Ce jeu est une variante de \"Tout noir ou tout blanc\" mais avec plusieurs couleurs."
        ,   H.br
        ,   H.text "Lorsque tu cliques un jeton, celui-ci change de couleurs ainsi que tous les jetons proches jusqu'à la distance choisie dans \"Portée\"."
        ,   H.br
        ,   H.text "Le but est que tous les jetons soient de couleur verte."
        ]
        
    