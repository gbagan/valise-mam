module Game.Tiling.View (view) where
import MyPrelude
import Lib.Util (coords)
import Pha (VDom, Prop, text, ifN, maybeN)
import Pha.Html (div, key, attr, style, class', onclick, oncontextmenu', onpointerenter, onpointerleave)
import Pha.Event (preventDefault)
import Pha.Svg (svg, g, rect, line, use, viewBox, fill, stroke, strokeWidth, transform)
import Pha.Util (translate)
import Game.Effs (EFFS)
import Game.Common (_isoCustom)
import Game.Core (_position, _nbRows, _nbColumns, _pointer, _help)
import Game.Tiling.Model (State, TileType(..), _nbSinks, _rotation, _tile, _tileType,
                          sinks, inConflict, setNbSinksA, setTileA, clickOnCellA, rotateA, flipTileA, setHoverSquareA)
import UI.Template (template, card, dialog, incDecGrid, gridStyle, svgCursorStyle, trackPointer)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, iconSelectGroup, ihelp, ireset, irules)

type Borders = {top :: Boolean, left :: Boolean, bottom :: Boolean, right :: Boolean}

square :: ∀a effs. {isDark :: Boolean, hasBlock :: Boolean, hasSink :: Boolean, row :: Int, col :: Int} -> Array (Prop a effs) -> VDom a effs 
square {isDark, hasBlock, hasSink, row, col} props =
    g ([
        class' "tiling-darken" isDark,
        transform $ translate (50 * col) (50 * row)
    ] <> props) [
        rect 0.0 0.0 50.0 50.0 [key "conc", fill "url(#concrete)"],
        ifN hasBlock \_ ->
            use 0.0 0.0 50.0 50.0 "#tile2" [key "tile"],
        ifN hasSink \_ ->
            use 0.0 0.0 50.0 50.0 "#sink" [key "sink"]
    ]
    
view :: State -> VDom State EFFS
view state = template _{config=config, board=board, rules=rules, winTitle=winTitle, customDialog=customDialog} state where
    position = state^._position
    rows = state^._nbRows
    columns = state^._nbColumns

    border i di = position !! i /= position !! (i + di)


    config = card "Carrelage" [
        iconSizesGroup state [4∧5, 5∧5, 5∧6, 7∧7] true,
        iconSelectGroup state "Motif du pavé" [Type1, Type2, Type3, CustomTile] (state^._tileType) setTileA \t ->
            _{icon = IconSymbol ("#" <> show t)},  --- custom
        iconSelectGroup state "Nombre d'éviers" [0, 1, 2] (state^._nbSinks) setNbSinksA (const identity),
        icongroup "Options" [ihelp state, ireset state, irules state]
    ]

    tileCursor pp =
        g (svgCursorStyle pp) [
            g [
                class' "tiling-cursor" true,
                style "transform" $ "rotate(" <> show (90 * state^._rotation) <> "deg)"
            ] $ state^._tile <#> \{row, col} ->
                use (50.0 * toNumber col - 25.0) (50.0 * toNumber row - 25.0) 50.0 50.0 "#tile2" [
                    attr "pointer-events" "none",
                    attr "opacity" (if inConflict state then "0.3" else "0.8")
                ]
        ]
        
    sinkCursor pp =
        use (-25.0) (-25.0) (50.0) (50.0) "#sink" ([
            attr "pointer-events" "none"
        ] <> svgCursorStyle pp)

    grid = div (gridStyle rows columns 5 <> trackPointer <> [
        class' "ui-board" true,
        -- todo
        oncontextmenu' \ev -> preventDefault ev *> rotateA
    ]) [
        svg [viewBox 0 0 (50 * columns) (50 * rows)] $
            (position # mapWithIndex \index pos ->
                let {row, col} = coords columns index in
                square {
                    isDark: state^._help && even (row + col),
                    hasBlock: pos > 0,
                    hasSink: pos == -1,
                    row, col
                } [
                    onclick $ clickOnCellA index,
                    onpointerenter $ setHoverSquareA (Just index),
                    onpointerleave $ setHoverSquareA Nothing
                ]
            ) <> (position # mapWithIndex \index pos ->
                let {row, col} = coords columns index in
                g [transform $ translate (50 * col) (50 * row)] [
                    ifN (pos > 0 && border index (-1)) \_ ->
                        line 0.0 0.0 0.0 50.0 [stroke "#000", strokeWidth "2"],
                    ifN (pos > 0 && border index 1) \_ ->
                        line 50.0 0.0 50.0 50.0 [stroke "#000", strokeWidth "2"],
                    ifN (pos > 0 && border index (-columns)) \_ ->
                        line 0.0 0.0 50.0 0.0 [stroke "#000", strokeWidth "2"],
                    ifN (pos > 0 && border index columns) \_ ->
                        line 0.0 50.0 50.0 50.0 [stroke "#000", strokeWidth "2"]    
                ]
            ) <> [maybeN $ (if length (sinks state) < state^._nbSinks then sinkCursor else tileCursor) <$> state^._pointer]
    ]

    board = incDecGrid state [grid]

    rules = [text "blah blah"]
    winTitle = "GAGNÉ"

    customDialog _ = dialog "Personnalise ta tuile" [
        div [class' "tiling-customtile-grid-container" true] [
            div [class' "tiling-grid" true] [
                svg [viewBox 0 0 250 250] (
                    state^. (_tile ∘ _isoCustom) # mapWithIndex \index hasBlock ->
                        let {row, col} = coords 5 index
                        in square {hasBlock, row, col, hasSink: false, isDark: false}
                            [key (show index), onclick $ flipTileA index]
                )
            ]
        ]
    ]

        {-
        I.Icon({
                    symbol: 'cup',
                    tooltip: 'Succès',
                    onclick: [actions.showDialog, 'success']

    const HelpDialog = () => C.HelpDialog(
        'Essaie de remplir tout le plateau avec des pavés respectant un certain motif.', br,
        'Utilise le clic droit ou la barre espace pour tourner le pavé', br,
        'Dans les options, tu peux choisir d\'utiliser des éviers.', br,
        'Ceux-ci ne peuvent pas être déplacés et ne peuvent pas être carrelés.'
    );

    const SuccessDialog = () =>
        Dialog({
            title: 'Succès',
            onOk: [actions.showDialog, null]
        },
            div({ class: 'ui-flex-center tiling-success-container' },
                div({
                    class: 'tiling-grid',
                    style: gridStyle(state.rows, state.columns)
                },
                    state.successForThisConf.map(success =>
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
