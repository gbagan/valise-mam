module Game.Jetons.View (view) where

import MyPrelude
import Pha (VDom, text, (<&&>), maybeN, key, class_, class', style)
import Pha.Elements (div, span, br)
import Pha.Util (pc, rgbColor)
import Game.Core (_position, _nbColumns, _nbRows, _pointer, scoreFn)
import Game.Jetons.Model (State, Msg, _dragged)
import Lib.Util (coords)
import UI.Template (template, card, bestScoreDialog, incDecGrid, gridStyle, dndBoardProps, dndItemProps, cursorStyle)
import UI.Icons (icongroup, iconBestScore, iconSizesGroup, iundo, iredo, ireset, irules)

view ∷ State → VDom Msg
view state = template {config, board, rules, winTitle, scoreDialog} state where
    position = state ^. _position
    columns = state ^. _nbColumns
    rows = state ^. _nbRows
    dragged = state ^. _dragged
    pointer = state ^. _pointer

    config = card "Jeu des jetons"
        [    iconSizesGroup state [2∧2, 4∧4, 5∧5, 6∧6] true
        ,    icongroup "Options" $ [iundo, iredo, ireset, irules] <#> (_ $ state)
        ,    iconBestScore state
        ]

    cursor pp _ = div ([class_ "ui-cursor jetons-cursor"] <> cursorStyle pp rows columns 0.6) []

    piece i val props =
        let {row, col} = coords columns i in
        div (
        [   class_ "jetons-peg"
        ,   class' "small" $ columns >= 8
        ,   style "background-color" $ rgbColor 255 (floor $ 255.0 * (1.0 - sqrt (toNumber val / toNumber (rows * columns)))) 0
        ,   style "left" $ pc $ (0.15 + toNumber col) / toNumber columns
        ,   style "top" $ pc $ (0.15 + toNumber row) / toNumber rows
        ,   style "width" $ pc $ 0.7 / toNumber columns
        ,   style "height" $ pc $ 0.7 / toNumber rows
        ,   style "box-shadow" $ show (val * 2) <> "px " <> show(val * 2) <> "px 5px 0px #656565"
        ] <> props) [ span [] [text $ show val] ]

    board = incDecGrid state [
        div ([class_ "ui-board"] <> dndBoardProps <> gridStyle rows columns 3) $ concat
        [   position # mapWithIndex \i val →
                val ≠ 0 <&&> \_ →
                    piece i val ([key $ show i] <> 
                        dndItemProps state
                        {   currentDragged: dragged
                        ,   draggable: true
                        ,   droppable: true
                        ,   id: i
                        })
        ,   [maybeN $ cursor <$> pointer <*> dragged]
        ]
    ]

    scoreDialog _ = bestScoreDialog state \pos → [
        div [class_ "ui-flex-center jetons-bestscore-grid-container"] [ 
            div (gridStyle rows columns 3 <> [class_ "ui-board"]) (
                pos # mapWithIndex \i val → val ≠ 0 <&&> \_ →
                    piece i val [key $ show i]
            )
        ]
    ]

    rules = 
        [   text "À chaque tour de ce jeu, tu peux déplacer une pile de jetons vers une case adjacente qui contient au moins autant de jetons."
        ,   br
        ,   text "Le but est de finir la partie avec le moins de cases contenant des piles de jetons."
    ]

    score = scoreFn state
    s = if score > 1 then "s" else ""
    winTitle = show score <> " case" <> s <> " restante" <> s