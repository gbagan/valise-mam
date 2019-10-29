module Game.Solitaire.View where

import Prelude
import Data.Int (toNumber)
import Data.Lens (Lens', (^.))
import Data.Array (mapWithIndex, filter, length)
import Data.Maybe (Maybe(..), maybe, isJust)
import Math (sin, cos, pi)
import Lib.Core (coords)
import Pha.Class (VDom)
import Pha (text, emptyNode)
import Pha.Action ((🎲))
import Pha.Html (div', br, svg, circle, key, attr, class', style, width, height, viewBox, fill, stroke, strokeWidth, pointermove, translate)
import Game.Core (_position, _nbColumns, _nbRows, _pointerPosition)
import Game.Solitaire.Model (SolitaireState, Board(..), _board, _holes, _dragged, setBoardA)
import UI.Dialog (card)
import UI.Icon (icongroup, Icon(..))
import UI.Icons (iconSelect, iundo, iredo, ireset, irules)
import UI.Template (template, gridStyle, incDecGrid, setPointerPositionA, svgCursorStyle, dndBoardProps, dndItemProps)

tricolor :: Int -> Int -> Int -> String
tricolor i columns help = 
    case (i `mod` columns + help * (i / columns)) `mod` 3 of
        0 -> "red"
        1 -> "blue"
        _ -> "green"

view :: forall a. Lens' a SolitaireState -> SolitaireState -> VDom a
view lens state = template lens {config, board, rules, winTitle} state where
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
        \pos -> circle 0.0 0.0 20.0 ([attr "pointer-events" "none", fill "url(#soli-peg)"] <> (svgCursorStyle $ pos))

    config =
        let ic = iconSelect lens state (state^._board) setBoardA in
        card "Jeu du solitaire" [
            icongroup "Plateau" [
                ic CircleBoard (_{icon = IconSymbol "#circle", tooltip = Just "Cercle"}),
                ic Grid3Board (_{icon = IconText "3xN", tooltip = Just "3xN"}), 
                ic RandomBoard (_{icon = IconSymbol "#shuffle", tooltip = Just "Aléatoire"}), 
                ic EnglishBoard  (_{icon = IconSymbol "#bread", tooltip = Just "Anglais"}),
                ic FrenchBoard (_{icon = IconSymbol "#tea", tooltip = Just "Français"}) 
            ],
            icongroup "Options" $ [iundo, iredo, ireset, irules] <#> \x -> x lens state     --- help
        ] 

    grid = div' ([
        pointermove $ lens 🎲 setPointerPositionA,
        class' "ui-board" true
    ] <> dndBoardProps lens _dragged <> (if isCircleBoard then [style "width" "100%", style "height" "100%"]  else gridStyle rows columns)) [
        svg [width "100%", height "100%",
            viewBox $ if isCircleBoard then "0 0 250 250" else "0 0 " <> show (50 * columns) <> " " <> show (50 * rows)
        ] $
            (if isCircleBoard then [circle 125.0 125.0 90.0 [stroke "grey", fill "transparent", strokeWidth "5"]] else [])
            <> (state^._holes # mapWithIndex \i val -> if not val then emptyNode else
                --rect (-25.0) (-25.0) 50.0 50.0 [fill: state.help > 0 && state.boardName !== 'circle' ? tricolor(i, state.columns, state.help) : 'transparent',
                --    transform: itemStyle(i),
                --}),
                circle 0.0 0.0 17.0 ([
                    key $ "h" <> show i,
                    fill "url(#soli-hole)",
                    class' "solitaire-hole" true,
                    style "transform" $ itemStyle i
                ] <> dndItemProps lens _dragged false true i state)
            ) <> (state^._position # mapWithIndex \i val -> if not val then emptyNode else
                circle 0.0 0.0 20.0 ([
                    key $ "p" <> show i,
                    fill "url(#soli-peg)",
                    class' "solitaire-peg" true,
                    style "transform" $ itemStyle i
                ] <> dndItemProps lens _dragged true false i state)
            ) <> (if isJust (state^._dragged) then [cursor] else [])
    ]

    board = incDecGrid lens state [grid]

    rules = [text "Jeu du solitaire", br, text "blah blah"]

    nbPegs = state^._position # filter identity # length
    s = if nbPegs > 1 then "s" else ""
    winTitle = show nbPegs <> " jeton" <> s <> " restant" <> s