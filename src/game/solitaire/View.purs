module Game.Solitaire.View where

import MyPrelude
import Lib.Util (coords)
import Pha (VDom, text, (<&&>), maybeN, key, attr, class_, class', style)
import Pha.Elements (div, br)
import Pha.Svg (svg, rect, circle, viewBox, x_, y_, width, height, cx, cy, r, fill, stroke, strokeWidth)
import Pha.Util (px, translate)
import Game.Core (PointerPosition, _position, _nbColumns, _nbRows, _pointer, scoreFn)
import Game.Solitaire.Model (State, Msg(..), Board(..), _board, _holes, _dragged, _help)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup, iconBestScore, ihelp, iundo, iredo, ireset, irules)
import UI.Template (template, card, bestScoreDialog, gridStyle, incDecGrid, svgCursorStyle, dndBoardProps, dndItemProps)

tricolor ∷ Int → Int → Int → String
tricolor i columns help = 
    case (i `mod` columns + help * (i / columns)) `mod` 3 of
        0 → "red"
        1 → "blue"
        _ → "green"

cursor ∷ ∀a b. PointerPosition → b → VDom a
cursor pp _ = circle ([r "20", attr "pointer-events" "none", fill "url(#soli-peg)"] <> svgCursorStyle pp)

view ∷ State → VDom Msg
view state = template {config, board, rules, winTitle, scoreDialog} state where
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
        let boards = [CircleBoard, Grid3Board, RandomBoard, EnglishBoard, FrenchBoard] in        
        card "Jeu du solitaire"
        [   iconSelectGroup state "Plateau" boards (state^._board) SetBoard \i opt → case i of
                CircleBoard → opt{icon = IconSymbol "#circle", tooltip = Just "Cercle"}
                Grid3Board → opt{icon = IconText "3xN", tooltip = Just "3xN"}
                RandomBoard → opt{icon = IconSymbol "#shuffle", tooltip = Just "Aléatoire"}
                EnglishBoard → opt{icon = IconSymbol "#tea", tooltip = Just "Anglais"}
                FrenchBoard →  opt{icon = IconSymbol "#bread", tooltip = Just "Français"}
        ,   icongroup "Options" $ [ihelp, iundo, iredo, ireset, irules] <#> \x → x state
        ,   iconBestScore state
        ]

    drawHole i = 
        [   state^._help > 0 && not isCircleBoard <&&> \_ →
                rect
                [   x_ "-25.0"
                ,   y_ "-25"
                ,   width "50"
                ,   height "50"
                ,   key $ "rect" <> show i
                ,   fill $ tricolor i columns (state^._help)
                ,   style "transform" $ itemStyle i
                ]
        ,   circle (
            [   key $ "h" <> show i
            ,   r "17"
            ,   fill "url(#soli-hole)"
            ,   class' "solitaire-hole" true
            ,   style "transform" $ itemStyle i
            ] <> dndItemProps 
                {   currentDragged: state^._dragged
                ,   draggable: false
                ,   droppable: true
                ,   id: i
                } state
            )
        ]

    drawPeg i =
        circle (
        [   r "20"
        ,   key $ "p" <> show i
        ,   fill "url(#soli-peg)"
        ,   class' "solitaire-peg" true
        ,   style "transform" $ itemStyle i
        ] <> dndItemProps
            {   draggable: true
            ,   droppable: false
            ,   currentDragged: state^._dragged
            ,   id: i
            } state
        )


    grid =
        div ([
            class' "ui-board" true
        ] <> dndBoardProps <> (if isCircleBoard then
                            [style "width" "100%", style "height" "100%"] 
                        else 
                            gridStyle rows columns 5
        ))
        [   svg [if isCircleBoard then viewBox 0 0 250 250 else viewBox 0 0 (50 * columns) (50 * rows)] $ concat
            [   [isCircleBoard <&&> \_ →
                    circle [cx "125", cy "125", r "90", stroke "grey", fill "transparent", strokeWidth "5"]
                ]
            ,   concat $ state^._holes # mapWithIndex \i hasHole →
                    if hasHole then drawHole i else []
            ,   state^._position # mapWithIndex \i hasPeg →
                    hasPeg <&&> \_ → drawPeg i
            ,   [maybeN $ cursor <$> state^._pointer <*> state^._dragged]
            ]
        ]

    board = incDecGrid state [grid]

    scoreDialog _ = bestScoreDialog state \position → [
        div [class' "ui-flex-center solitaire-scoredialog" true] [
            div([class' "ui-board" true] <> (if isCircleBoard then 
                                    [style "width" "100%", style "height" "100%"] 
                                else 
                                    gridStyle rows columns 5
            )) [
                svg [if isCircleBoard then viewBox 0 0 250 250 else viewBox 0 0 (50 * columns) (50 * rows)] $ concat [
                    [isCircleBoard <&&> \_ →
                        circle [cx "125", cy "125", r "90", stroke "grey", fill "transparent", strokeWidth "5"]
                    ],
                    state^._holes # mapWithIndex \i → (_ <&&> \_ →
                        circle [
                            key $ "h" <> show i,
                            r "17",
                            fill "url(#soli-hole)",
                            class_ "solitaire-hole",
                            style "transform" $ itemStyle i
                        ]
                    ),
                    position # mapWithIndex \i → (_ <&&> \_ →
                        circle [
                            key $ "p" <> show i,
                            r "20",
                            fill "url(#soli-peg)",
                            class_ "solitaire-peg",
                            style "transform" $ itemStyle i
                        ]
                    )
                ]
            ]
        ]
    ]


    rules = [text "Jeu du solitaire", br, text "blah blah"]

    nbPegs = scoreFn state
    s = if nbPegs > 1 then "s" else ""
    winTitle = show nbPegs <> " jeton" <> s <> " restant" <> s