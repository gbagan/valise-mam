module Game.Tricolor.View where

import MyPrelude
import Pha (VDom, text)
import Pha.Action ((🔍))
import Pha.Html (div', svg, circle, class', key, style, click, pointerenter, pointerleave, pc, stroke, fill, viewBox, translate)
import Game.Effs (EFFS)
import Game.Core (playA, isLevelFinished, _position)
import Game.Tricolor.Model (State, setSizeA, setNbColorsA, setRangeA, setHoverCellA, _size, _nbColors, _range, _hoverCell, inRange)
import UI.Template (template, card)
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)

colors :: Array String
colors = ["green", "yellow", "red", "magenta", "blue"]

translateCell :: Int -> Int -> String
translateCell i size = translate (pc x) (pc y)  where
    x = 0.50 + 0.35 * cos (toNumber i * 2.0 * pi / toNumber size)
    y = 0.50 + 0.35 * sin (toNumber i * 2.0 * pi / toNumber size)

view :: ∀a. Lens' a State -> State -> VDom a EFFS
view lens state = template lens (_{config=config, board=board, rules=rules}) state where
    size = state^._size
    levelFinished = isLevelFinished state

    config = card "Feux tricolores" [
        iconSelectGroup lens state "Nombre de lumières" [4, 5, 6, 7, 8] size setSizeA (const identity),
        iconSelectGroup lens state "Nombre de couleurs" [2, 3, 4, 5] (state^._nbColors) setNbColorsA (const identity),
        iconSelectGroup lens state "Portée" [1, 2, 3] (state^._range) setRangeA (const identity),
        icongroup "Options" $ [ iundo, iredo, ireset, irules ] <#> \x -> x lens state
    ]

    board = div' [class' "ui-board tricolor-board" true] [
        svg [viewBox 0 0 100 100] ( 
            state^._position # mapWithIndex \i color ->
                circle 0.0 0.0 7.5 [
                    class' "tricolor-cell" true,
                    class' "finished" levelFinished,
                    stroke $ if maybe false (inRange state i) (state^._hoverCell) then "lightgreen" else "",
                    key $ "b" <> show i,
                    style "fill" $ if levelFinished then "" else colors !! color # fromMaybe "",
                    style "transform" (translateCell i size),
                    click $ lens 🔍 playA i,
                    pointerenter $ lens 🔍 setHoverCellA (Just i),
                    pointerleave $ lens 🔍 setHoverCellA Nothing
                ]
        )
    ]

    rules = [text "blah blah blah blah"]
    