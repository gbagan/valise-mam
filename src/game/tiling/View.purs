module Game.Tiling.View (view) where
import MyPrelude
import Lib.Util (coords)
import Pha (VDom, Prop, text, (<&&>), (<??>), key, attr, style, class')
import Pha.Elements (div)
import Pha.Events (onclick, oncontextmenu, onpointerenter, onpointerleave)
import Pha.Svg (svg, g, rect, line, use,
                viewBox, fill, stroke, x_, y_, x1, x2, y1, y2, width, height, strokeWidth, transform)
import Pha.Util (translate)
import Game.Common (_isoCustom)
import Game.Core (_position, _nbRows, _nbColumns, _pointer, _help)
import Game.Tiling.Model (State, Msg(..), TileType(..), _nbSinks, _rotation, _tile, _tileType,
                          sinks, needSinks, inConflict)
import UI.Template (template, card, dialog, incDecGrid, gridStyle, svgCursorStyle, trackPointer)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, iconSelectGroup, ihelp, ireset, irules)

type Borders = {top ∷ Boolean, left ∷ Boolean, bottom ∷ Boolean, right ∷ Boolean}

square ∷ ∀a. {isDark ∷ Boolean, hasBlock ∷ Boolean, hasSink ∷ Boolean, row ∷ Int, col ∷ Int} → Array (Prop a) → VDom a 
square {isDark, hasBlock, hasSink, row, col} props =
    g ([
        class' "tiling-darken" isDark,
        transform $ translate (show $ 50 * col) (show $ 50 * row)
    ] <> props) [
        rect [width "50", height "50", key "conc", fill "url(#concrete)"],
        hasBlock <&&> \_ →
            use "#tile2" [width "50", height "50", key "tile"],
        hasSink <&&> \_ →
            use "#sink" [width "50", height "50", key "sink"]
    ]
    
view ∷ State → VDom Msg
view state = template {config, board, rules, winTitle, customDialog} state where
    position = state^._position
    rows = state^._nbRows
    columns = state^._nbColumns

    border i di = position !! i /= position !! (i + di)


    config =
        card "Carrelage"
        [   iconSizesGroup state [4∧5, 5∧5, 5∧6, 7∧7] true
        ,   iconSelectGroup state "Motif du pavé" [Type1, Type2, Type3, CustomTile] (state^._tileType) SetTile \t →
                _{icon = IconSymbol ("#" <> show t)}
        ,   iconSelectGroup state "Nombre d'éviers" [0, 1, 2] (state^._nbSinks) SetNbSinks (const identity)
        ,   icongroup "Options" [ihelp state, ireset state, irules state]
        ]

    tileCursor pp =
        g (svgCursorStyle pp)
        [   g [
                class' "tiling-cursor" true,
                style "transform" $ "rotate(" <> show (90 * state^._rotation) <> "deg)"
            ] $ state^._tile <#> \{row, col} →
                use "#tile2"
                [   x_ $ show (50.0 * toNumber col - 25.0)
                ,   y_ $ show (50.0 * toNumber row - 25.0)
                ,   width "50"
                ,   height "50"
                ,   attr "pointer-events" "none"
                ,   attr "opacity" (if inConflict state then "0.3" else "0.8")
                ]
        ]
        
    sinkCursor pp =
        use "#sink" ([
            x_ "-25", y_ "-25", width "50", height "50",
            attr "pointer-events" "none"
        ] <> svgCursorStyle pp)

    grid =
        div (gridStyle rows columns 5 <> trackPointer <> [
            class' "ui-board" true,
            oncontextmenu Rotate
        ]) [
            svg [viewBox 0 0 (50 * columns) (50 * rows)] $ concat
            [   position # mapWithIndex \index pos →
                    let {row, col} = coords columns index in
                    square
                    {   isDark: state^._help && even (row + col)
                    ,   hasBlock: pos > 0
                    ,   hasSink: pos == -1
                    ,   row
                    ,   col
                    }
                    [   onclick $ if needSinks state then PutSink index else Play index
                    ,   onpointerenter $ SetHoverSquare (Just index)
                    ,   onpointerleave $ SetHoverSquare Nothing
                    ]
            ,   position # mapWithIndex \index pos →
                    let {row, col} = coords columns index in
                    g [transform $ translate (show $ 50 * col) (show $ 50 * row)]
                    [   pos > 0 && border index (-1) <&&> \_ →
                            line [x1 "0", y1 "0", x2 "0", y2 "50", stroke "#000", strokeWidth "2"]
                    ,   pos > 0 && border index 1 <&&> \_ →
                            line [x1 "50", y1 "0", x2 "50", y2 "50", stroke "#000", strokeWidth "2"]
                    ,   pos > 0 && border index (-columns) <&&> \_ →
                            line [x1 "0", y1 "0", x2 "50", y2 "0", stroke "#000", strokeWidth "2"]
                    ,   pos > 0 && border index columns <&&> \_ →
                            line [x1 "0", y1 "50", x2 "50", y2 "50", stroke "#000", strokeWidth "2"]    
                    ]
            ,   [state^._pointer <??> (if length (sinks state) < state^._nbSinks then sinkCursor else tileCursor)]
            ]
        ]

    board = incDecGrid state [grid]

    customDialog _ = dialog "Personnalise ta tuile" [
        div [class' "tiling-customtile-grid-container" true] [
            div [class' "tiling-grid" true] [
                svg [viewBox 0 0 250 250] (
                    state^. (_tile ∘ _isoCustom) # mapWithIndex \index hasBlock →
                        let {row, col} = coords 5 index
                        in square {hasBlock, row, col, hasSink: false, isDark: false}
                            [key (show index), onclick $ FlipTile index]
                )
            ]
        ]
    ]

    rules =
        [   text "blah blah"
        ]

    winTitle = "GAGNÉ"

        {-
        I.Icon({
                    symbol: 'cup',
                    tooltip: 'Succès',
                    onclick: [actions.showDialog, 'success']

    const HelpDialog = () ⇒ C.HelpDialog(
        'Essaie de remplir tout le plateau avec des pavés respectant un certain motif.', br,
        'Utilise le clic droit ou la barre espace pour tourner le pavé', br,
        'Dans les options, tu peux choisir d\'utiliser des éviers.', br,
        'Ceux-ci ne peuvent pas être déplacés et ne peuvent pas être carrelés.'
    );

    const SuccessDialog = () ⇒
        Dialog({
            title: 'Succès',
            onOk: [actions.showDialog, null]
        },
            div({ class: 'ui-flex-center tiling-success-container' },
                div({
                    class: 'tiling-grid',
                    style: gridStyle(state.rows, state.columns)
                },
                    state.successForThisConf.map(success ⇒
                        Square({
                            hasSink: success,
                            style: {
                                width: 100 / state.columns + '%',
                                height: 100 / state.rows + '%'
                            }
                        })
                    )
                )
            )
        );
