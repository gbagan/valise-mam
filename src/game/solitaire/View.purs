module Game.Solitaire.View where

import MyPrelude
import Lib.Util (coords)
import Pha (VDom, text, ifN, maybeN)
import Pha.Action ((🔍))
import Pha.Html (div', br, svg, rect, circle, key, attr, class', style, click, 
                    viewBox, fill, stroke, strokeWidth, translate, px)
import Game.Effs (EFFS)
import Game.Core (PointerPosition, _position, _nbColumns, _nbRows, _pointer, scoreFn)
import Game.Solitaire.Model (State, Board(..), _board, _holes, _dragged, _help, setBoardA, toggleHelpA)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSelectGroup, iconBestScore, iundo, iredo, ireset, irules)
import UI.Template (template, card, bestScoreDialog, gridStyle, incDecGrid, svgCursorStyle, dndBoardProps, dndItemProps)

tricolor :: Int -> Int -> Int -> String
tricolor i columns help = 
    case (i `mod` columns + help * (i / columns)) `mod` 3 of
        0 -> "red"
        1 -> "blue"
        _ -> "green"

cursor :: ∀a b. PointerPosition -> b -> VDom a EFFS
cursor pp _ = circle 0.0 0.0 20.0 ([attr "pointer-events" "none", fill "url(#soli-peg)"] <> svgCursorStyle pp)

view :: ∀a. Lens' a State -> State -> VDom a EFFS
view lens state = template lens (_{config=config, board=board, rules=rules, winTitle=winTitle, scoreDialog=scoreDialog}) state where
    columns = state^._nbColumns
    rows = state^._nbRows
    isCircleBoard = state^._board == CircleBoard

    itemStyle i = 
        let {row, col} = coords columns i in
        if isCircleBoard then
            translate
                (px $ 125.0 + sin(2.0 * pi * toNumber i / toNumber rows) * 90.0)
                (px $ 125.0 + cos(2.0 * pi * toNumber i / toNumber rows) * 90.0)
        else
            translate (px $ 50.0 * toNumber col + 25.0) (px $ 50.0 * toNumber row + 25.0)

    config =
        let boards = [CircleBoard, Grid3Board, RandomBoard, EnglishBoard, FrenchBoard]
            ihelp = iconbutton state (_{icon = IconSymbol "#help", selected = state^._help > 0, tooltip = Just "Aide"})
                    [click $ lens 🔍 toggleHelpA]
        in        
        card "Jeu du solitaire" [
            iconSelectGroup lens state "Plateau" boards (state^._board) setBoardA \i opt -> case i of
                CircleBoard -> opt{icon = IconSymbol "#circle", tooltip = Just "Cercle"}
                Grid3Board -> opt{icon = IconText "3xN", tooltip = Just "3xN"}
                RandomBoard -> opt{icon = IconSymbol "#shuffle", tooltip = Just "Aléatoire"}
                EnglishBoard -> opt{icon = IconSymbol "#tea", tooltip = Just "Anglais"}
                FrenchBoard ->  opt{icon = IconSymbol "#bread", tooltip = Just "Français"},
            icongroup "Options" $ [ihelp] <> ([iundo, iredo, ireset, irules] <#> \x -> x lens state),
            iconBestScore lens state
        ] 

    grid = div' ([
        class' "ui-board" true
    ] <> dndBoardProps lens _dragged <> (if isCircleBoard then
                            [style "width" "100%", style "height" "100%"] 
                        else 
                            gridStyle rows columns 5
    )) [
        svg [if isCircleBoard then viewBox 0 0 250 250 else viewBox 0 0 (50 * columns) (50 * rows)] $ concat [
            [ifN isCircleBoard \_ ->
                circle 125.0 125.0 90.0 [stroke "grey", fill "transparent", strokeWidth "5"]
            ],
            concat $ state^._holes # mapWithIndex \i val -> if not val then [] else [
                ifN (state^._help > 0 && not isCircleBoard) \_ ->
                    rect (-25.0) (-25.0) 50.0 50.0 [
                        key $ "rect" <> show i,
                        fill $ tricolor i columns (state^._help),
                        style "transform" $ itemStyle i
                    ],
                circle 0.0 0.0 17.0 ([
                    key $ "h" <> show i,
                    fill "url(#soli-hole)",
                    class' "solitaire-hole" true,
                    style "transform" $ itemStyle i
                ] <> dndItemProps lens _dragged false true i state)
            ],
            state^._position # mapWithIndex \i val -> ifN val \_ ->
                circle 0.0 0.0 20.0 ([
                    key $ "p" <> show i,
                    fill "url(#soli-peg)",
                    class' "solitaire-peg" true,
                    style "transform" $ itemStyle i
                ] <> dndItemProps lens _dragged true false i state),
            [maybeN $ cursor <$> state^._pointer <*> state^._dragged]
        ]
    ]

    board = incDecGrid lens state [grid]

    scoreDialog _ = bestScoreDialog lens state \position -> [
        div' [class' "ui-flex-center solitaire-scoredialog" true] [
            div'([class' "ui-board" true] <> (if isCircleBoard then 
                                    [style "width" "100%", style "height" "100%"] 
                                else 
                                    gridStyle rows columns 5
            )) [
                svg [if isCircleBoard then viewBox 0 0 250 250 else viewBox 0 0 (50 * columns) (50 * rows)] $ concat [
                    [ifN isCircleBoard \_ ->
                        circle 125.0 125.0 90.0 [stroke "grey", fill "transparent", strokeWidth "5"]
                    ],
                    state^._holes # mapWithIndex \i val -> ifN val \_ ->
                        circle 0.0 0.0 17.0 [
                            key $ "h" <> show i,
                            fill "url(#soli-hole)",
                            class' "solitaire-hole" true,
                            style "transform" $ itemStyle i
                        ],
                    position # mapWithIndex \i val -> ifN val \_ ->
                        circle 0.0 0.0 20.0 [
                            key $ "p" <> show i,
                            fill "url(#soli-peg)",
                            class' "solitaire-peg" true,
                            style "transform" $ itemStyle i
                        ]
                ]
            ]
        ]
    ]


    rules = [text "Jeu du solitaire", br, text "blah blah"]

    nbPegs = scoreFn state
    s = if nbPegs > 1 then "s" else ""
    winTitle = show nbPegs <> " jeton" <> s <> " restant" <> s