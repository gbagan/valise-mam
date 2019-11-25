module Game.Tricolor.View where

import MyPrelude
import Pha (VDom, text, class', attr, key, style)
import Pha.Elements (div)
import Pha.Attributes (onclick, onpointerenter, onpointerleave)
import Pha.Svg (svg, circle, text', stroke, fill, viewBox, cx, cy, r)
import Pha.Util (pc, translate)
import Game.Effs (EFFS)
import Game.Core (playA, isLevelFinished, _position)
import Game.Tricolor.Model (State, setSizeA, setNbColorsA, setRangeA, setHoverCellA, _size, _nbColors, _range, _hoverCell, inRange)
import UI.Template (template, card)
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)

colors :: Array String
colors = ["green", "yellow", "red", "magenta", "blue"]

translateCell :: Int -> Int -> String
translateCell i size = translate (pc x) (pc y) where
    x = 0.50 + 0.35 * cos (toNumber i * 2.0 * pi / toNumber size)
    y = 0.45 + 0.35 * sin (toNumber i * 2.0 * pi / toNumber size)

view :: State -> VDom State EFFS
view state = template _{config=config, board=board, rules=rules} state where
    size = state^._size
    nbColors = state^._nbColors
    levelFinished = isLevelFinished state

    config = card "Feux tricolores" [
        iconSelectGroup state "Nombre de lumières" [4, 5, 6, 7, 8] size setSizeA (const identity),
        iconSelectGroup state "Nombre de couleurs" [2, 3, 4, 5] nbColors setNbColorsA (const identity),
        iconSelectGroup state "Portée" [1, 2, 3] (state^._range) setRangeA (const identity),
        icongroup "Options" $ [ iundo, iredo, ireset, irules ] <#> \x -> x state
    ]

    board = div [class' "ui-board tricolor-board" true] [
        svg [viewBox 0 0 100 100] $ concat [
            state^._position # mapWithIndex \i color ->
                circle [
                    r "7.5",
                    class' "tricolor-cell" true,
                    class' "finished" levelFinished,
                    stroke $ if (inRange state i <$> state^._hoverCell) == Just true then "lightgreen" else "black",
                    key $ "b" <> show i,
                    style "fill" $ if levelFinished then "" else colors !! color # fromMaybe "",
                    style "transform" (translateCell i size),
                    onclick $ playA i,
                    onpointerenter $ setHoverCellA (Just i),
                    onpointerleave $ setHoverCellA Nothing
                ],

            concat $ take nbColors colors # mapWithIndex \i color -> [
                circle [
                    cx $ show (95 + 15 * (i - nbColors)),
                    cy "95",
                    r "3",
                    key $ "c" <> show i,
                    fill color
                ],
                text' "➡" [
                    cx $ show (99 + 15 * (i - nbColors)),
                    cy "97",
                    key $ "t" <> show i,
                    attr "font-size" "7"
                ]
            ],
            [circle [cx "95", cy "95",  r "3", key "fc", fill "green"]]
        ]
    ]

    rules = [text "blah blah blah blah"]
    