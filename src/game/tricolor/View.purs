module Game.Tricolor.View where

import MyPrelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Game.Core (isLevelFinished, _position)
import Game.Tricolor.Model (State, Msg(..), _size, _nbColors, _range, _hoverCell, _shuffle, inRange)
import Pha as H
import Pha.Elements as HH
import Pha.Attributes as P
import Pha.Events as E
import Pha.Util (pc, translate)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSelectGroup, iundo, iredo, ireset, irules)
import UI.Template (template, card)

colors ∷ Array String
colors = ["green", "yellow", "red", "magenta", "blue"]

translateCell ∷ Int → Int → String
translateCell i size = translate (pc x) (pc y) where
    x = 0.50 + 0.35 * cos (toNumber i * 2.0 * pi / toNumber size)
    y = 0.45 + 0.35 * sin (toNumber i * 2.0 * pi / toNumber size)

irandom ∷ State → H.VDom Msg
irandom state =
    iconbutton
        state {icon: IconSymbol "#shuffle", tooltip: Just "Mélanger", selected: state^._shuffle}
        [   E.onclick Shuffle
        ]

view ∷ State → H.VDom Msg
view state = template {config, board, rules} state where
    position = state ^. _position
    size = state ^. _size
    nbColors = state ^. _nbColors
    range = state ^. _range
    levelFinished = isLevelFinished state
    hoverCell = state ^. _hoverCell

    config =
        card "Feux tricolores" 
        [   iconSelectGroup state "Nombre de lumières" [4, 5, 6, 7, 8] size SetSize (const identity)
        ,   iconSelectGroup state "Nombre de couleurs" [2, 3, 4, 5] nbColors SetNbColors (const identity)
        ,   iconSelectGroup state "Portée" [1, 2, 3] range SetRange (const identity)
        ,   icongroup "Options" $ [ iundo, iredo, ireset, irandom, irules ] <#> (_ $ state)
        ]

    drawCell i color =
        HH.circle 
        [   P.r 7.5
        ,   H.class_ "tricolor-cell"
        ,   H.class' "finished" levelFinished
        ,   P.stroke $ if (inRange state i <$> hoverCell) == Just true then "lightgreen" else "black"
        ,   H.key $ "b" <> show i
        ,   H.style "fill" $ if levelFinished then "" else colors !! color # fromMaybe ""
        ,   H.style "transform" (translateCell i size)
        ,   E.onclick $ Play i
        ,   E.onpointerenter $ SetHoverCell (Just i)
        ,   E.onpointerleave $ SetHoverCell Nothing
        ]

    drawColorCycle =
        (take nbColors colors # foldMapWithIndex \i color →
                [   HH.circle
                    [   P.cx $ toNumber (95 + 15 * (i - nbColors))
                    ,   P.cy 95.0
                    ,   P.r 3.0
                    ,   H.key $ "c" <> show i
                    ,   P.fill color
                    ]
                ,   HH.text "⮕"
                    [   P.x $ toNumber (99 + 15 * (i - nbColors))
                    ,   P.y 97.0
                    ,   H.key $ "t" <> show i
                    ,   H.attr "font-size" "7"
                    ]
                ]
        ) <> [HH.circle [P.cx 95.0, P.cy 95.0, P.r 3.0, H.key "fc", P.fill "green"]]

    board =
        HH.div [H.class_ "ui-board tricolor-board"]
        [   HH.svg [P.viewBox 0 0 100 100] $ concat
            [   position # mapWithIndex drawCell
            ,   drawColorCycle
            ]
        ]

    rules = 
        [   H.text "Ce jeu est une variante de \"Tout noir ou tout blanc\" mais avec plusieurs couleurs."
        ,   HH.br
        ,   H.text "Lorsque tu cliques un jeton, celui-ci change de couleurs ainsi que tous les jetons proches jusqu'à la distance choisie dans \"Portée\"."
        ,   HH.br
        ,   H.text "Le but est que tous les jetons soient de couleur verte."
        ]
        
    