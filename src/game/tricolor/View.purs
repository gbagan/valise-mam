module Game.Tricolor.View where

import MyPrelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Game.Core (isLevelFinished, _position)
import Game.Tricolor.Model (State, Msg(..), _size, _nbColors, _range, _hoverCell, inRange)
import Pha (VDom, attr, class', class_, key, style)
import Pha.Elements (div)
import Pha.Events (onclick, onpointerenter, onpointerleave)
import Pha.Svg (svg, circle, text', stroke, fill, viewBox, x_, y_, cx, cy, r)
import Pha.Util (pc, translate)
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)
import UI.Template (template, card)

colors ∷ Array String
colors = ["green", "yellow", "red", "magenta", "blue"]

translateCell ∷ Int → Int → String
translateCell i size = translate (pc x) (pc y) where
    x = 0.50 + 0.35 * cos (toNumber i * 2.0 * pi / toNumber size)
    y = 0.45 + 0.35 * sin (toNumber i * 2.0 * pi / toNumber size)

view ∷ State → VDom Msg
view state = template {config, board, rules} state where
    size = state^._size
    nbColors = state^._nbColors
    levelFinished = isLevelFinished state

    config =
        card "Feux tricolores" 
        [   iconSelectGroup state "Nombre de lumières" [4, 5, 6, 7, 8] size SetSize (const identity)
        ,   iconSelectGroup state "Nombre de couleurs" [2, 3, 4, 5] nbColors SetNbColors (const identity)
        ,   iconSelectGroup state "Portée" [1, 2, 3] (state^._range) SetRange (const identity)
        ,   icongroup "Options" $ [ iundo, iredo, ireset, irules ] <#> \x → x state
        ]

    drawCell i color =
        circle 
        [   r "7.5"
        ,   class_ "tricolor-cell"
        ,   class' "finished" levelFinished
        ,   stroke $ if (inRange state i <$> state^._hoverCell) == Just true then "lightgreen" else "black"
        ,   key $ "b" <> show i
        ,   style "fill" $ if levelFinished then "" else colors !! color # fromMaybe ""
        ,   style "transform" (translateCell i size)
        ,   onclick $ Play i
        ,   onpointerenter $ SetHoverCell (Just i)
        ,   onpointerleave $ SetHoverCell Nothing
        ]

    drawColorCycle =
        (take nbColors colors # foldMapWithIndex \i color →
                [   circle
                    [   cx $ show (95 + 15 * (i - nbColors))
                    ,   cy "95"
                    ,   r "3"
                    ,   key $ "c" <> show i
                    ,   fill color
                    ]
                ,   text' "⮕"
                    [   x_ $ show (99 + 15 * (i - nbColors))
                    ,   y_ "97"
                    ,   key $ "t" <> show i
                    ,   attr "font-size" "7"
                    ]
                ]
        ) <> [circle [cx "95", cy "95",  r "3", key "fc", fill "green"]]

    board =
        div [class_ "ui-board tricolor-board"]
        [   svg [viewBox 0 0 100 100] $ concat
            [   state^._position # mapWithIndex drawCell
            ,   drawColorCycle
            ]
        ]

    rules = []
        
    