module Game.Solitaire.View where

import Prelude
import Data.Int (toNumber)
import Data.Lens (Lens', (^.))
import Data.Array (mapWithIndex)
import Data.Maybe (maybe)
import Math (sin, cos, pi)
import Lib.Core (coords)
import Pha (VDom, text, emptyNode, (🎲))
import Pha.Html (div', br, svg, circle, key, class', style, width, height, viewBox, fill, stroke, strokeWidth, pointermove, translate)
import Game.Core (_position, _nbColumns, _nbRows, _pointerPosition)
import Game.Solitaire.Model (SolitaireState, Board(..), _board, _holes)
import UI.Dialog (card)
import UI.Icon (icongroup)
import UI.Icons (iundo, iredo, ireset, irules)
import UI.Template (template, gridStyle, incDecGrid, setPointerPositionA, svgCursorStyle, trackPointer)

tricolor :: Int -> Int -> Int -> String
tricolor i columns help = 
    case (i `mod` columns + help * (i / columns)) `mod` 3 of
        0 -> "red"
        1 -> "blue"
        _ -> "green"

view :: forall a. Lens' a SolitaireState -> SolitaireState -> VDom a
view lens state = template lens {config, board, rules} state where
    columns = state^._nbColumns
    rows = state^._nbRows
    isCircleBoard = state^._board == CircleBoard

    itemStyle i = 
        let {row, col} = coords columns i in
        if isCircleBoard then
            translate
                (125.0 + sin(2.0 * pi * toNumber i / toNumber rows) * 90.0)
                (125.0 + cos(2.0 * pi * toNumber i / toNumber rows) * 90.0)
        else
            translate (50.0 * toNumber col + 25.0) (50.0 * toNumber row + 25.0)

    cursor = state^._pointerPosition # maybe emptyNode 
        \pos -> circle 0.0 0.0 20.0 ([fill "url(#soli-peg)"] <> (svgCursorStyle $ pos))
            
--            'pointer-events': 'none'
--        });

    config = card "Jeu du solitaire" [
        {-    I.Group({
                title: 'Plateau',
                list: ['circle', 'grid', 'random', 'english', 'french',],
                symbol: ['circle', null, 'shuffle', 'tea', 'bread'],
                text: [null, '3xN'],
                tooltip: ['Cercle', 'Grille 3xN', 'Aléatoire', 'Anglais', 'Français'],
                select: state.boardName,
                onclick: actions.setBoard
            }),
        -}
        icongroup "Options" $ [iundo, iredo, ireset, irules] <#> \x -> x lens state     --- help
    ]

    grid = div' ([
        -- hasDnD: true,
        pointermove $ lens 🎲 setPointerPositionA,
        class' "ui-board" true
    ] <> trackPointer lens <> (if isCircleBoard then [style "width" "100%", style "height" "100%"]  else gridStyle rows columns)) [
        svg [width "100%", height "100%",
            viewBox $ if isCircleBoard then "0 0 250 250" else "0 0 " <> show (50 * columns) <> " " <> show (50 * rows)
        ] $
            (if isCircleBoard then [circle 125.0 125.0 90.0 [stroke "grey", fill "transparent", strokeWidth "5"]] else [])
            <> (state^._holes # mapWithIndex \i val -> if not val then emptyNode else
                --rect (-25.0) (-25.0) 50.0 50.0 [fill: state.help > 0 && state.boardName !== 'circle' ? tricolor(i, state.columns, state.help) : 'transparent',
                --    transform: itemStyle(i),
                --}),
                circle 0.0 0.0 17.0 [
                    key $ "h" <> show i,
                    fill "url(#soli-hole)",
                    class' "solitaire-hole" true,
                    --- droppable: true,
                    --- id: i,
                    style "transform" $ itemStyle i
                ]
            ) <> (state^._position # mapWithIndex \i val -> if not val then emptyNode else
                circle 0.0 0.0 20.0 [
                    key $ "p" <> show i,
                    fill "url(#soli-peg)",
                    class' "solitaire-peg" true,
                    --- draggable: true
                    -- id: i,
                    style "transform" $ itemStyle i
                ]
            ) <> [cursor]
            -- state.dragged !== null && state.pointerPosition && Cursor()
    ]

    board = incDecGrid lens state [grid]

    rules = [text "Jeu du solitaire", br, text "blah blah"]

    -- s = if state.nbPegs > 1 then "s" else ""


    --    winTitle: `${state.nbPegs} jeton${s} restant${s}`,
    --    Config, Board, HelpDialog