module Game.Tiling.View where
import Prelude
import Data.Tuple (Tuple(..))
import Data.Lens (Lens', (^.))
import Data.Int (toNumber, even)
import Data.Array (length, mapWithIndex)
import Lib.Util (tabulate, coords)
import Pha.Class (VDom, Prop)
import Pha (text, whenN, maybeN)
import Pha.Action ((🎲))
import Pha.Html (div', g, rect, use, key, attr, style, svg, class', click, contextmenu, width, height, viewBox, fill, transform)
import Game.Core (_position, _nbRows, _nbColumns, _pointerPosition)
import Game.Tiling.Model (TilingState, _nbSinks, _rotation, _tile, sinks, setNbSinksA, clickOnCellA, rotateA)
import UI.Template (template, card, incDecGrid, gridStyle, svgCursorStyle, trackPointer)
import UI.Icons (icongroup, iconSizesGroup, iconSelectGroup, ihelp, ireset, irules)

square :: forall a. {isBlack :: Boolean, hasBlock :: Boolean, hasSink :: Boolean, row :: Int, col :: Int} -> Array (Prop a) -> VDom a 
square {isBlack, hasBlock, hasSink, row, col} props =
    g ([
        class' "tiling-darken" isBlack,
        transform $ "translate(" <> show (50 * col) <> "," <> show (50 * row) <> ")"
    ] <> props) [
        rect 0.0 0.0 50.0 50.0 [key "conc", fill "url(#concrete)"],
        whenN hasBlock \_ ->
            use 0.0 0.0 50.0 50.0 "#tile2" [key "tile"],
        whenN hasSink \_ ->
            use 0.0 0.0 50.0 50.0 "#sink" [key "sink"]
    ]
    
view :: forall a. Lens' a TilingState -> TilingState -> VDom a
view lens state = template lens {config, board, rules, winTitle} state where
    rows = state^._nbRows
    columns = state^._nbColumns

    config = card "Carrelage" [
        iconSizesGroup lens state [Tuple 4 5, Tuple 5 5, Tuple 5 6, Tuple 7 7] true,
        {-     I.Group({
                title: 'Motif du pavé',
                list: [0, 1, 2, 'custom'],
                symbol: ['beast1', 'beast2', 'beast3', 'customize'],
                select: state.tileIndex,
                onclick: actions.setTile
            }),
        -}
        iconSelectGroup lens state "Nombre d'éviers" [0, 1, 2] (const identity) (state^._nbSinks) setNbSinksA,
        icongroup "Options" $ [ihelp, ireset, irules] <#> \x -> x lens state
    ]

    tileCursor pp =
        g (svgCursorStyle pp) [
            g [
                class' "tiling-cursor" true,
                style "transform" $ "rotate(" <> show (90 * state^._rotation) <> "deg)"
            ] $ state^._tile <#> \{row, col} ->
                use (50.0 * toNumber col - 25.0) (50.0 * toNumber row - 25.0) 50.0 50.0 "#tile2" [
                    attr "pointer-events" "none",
                    attr "opacity" "0.7" -- $ if state.inConflict ? 0.3 : 0.7
                ]
        ]

        
    sinkCursor pp =
        use (-25.0) (-25.0) (50.0) (50.0) "#sink" ([
            attr "pointer-events" "none"
        ] <> svgCursorStyle pp)

    grid = div' (gridStyle rows columns 5 <> trackPointer lens <> [
        class' "ui-board" true,
        contextmenu $ lens 🎲 rotateA  -- combine(actions.rotate, preventDefault),
    ]) [
        svg [width "100%", height "100%", viewBox $ "0 0 " <> show (50 * columns) <> " " <> show (50 * rows)] $
            (state^._position # mapWithIndex \index pos ->
                let {row, col} = coords columns index in
                square {
                    isBlack: false, --state.coloringVisible && even (row + col),
                    hasBlock: pos > 0,
                    hasSink: pos == -1,
                    row, col
                } [
                    click $ lens 🎲 clickOnCellA index
                --        borders: [-1, 1, -state.columns, state.columns].map(i =>
                --            state.position[index] > 0 && state.position[index] !== state.position[index + i]
                --        ),
                    -- 
                    --    onpointerenter: [actions.setHoverSquare, index],
                    --    onpointerleave: [actions.setHoverSquare, null]
                ])
            {- repeat2(state.rows, state.columns, (row, col, index) =>
                    g({
                        transform: `translate(${50 * col}, ${50 * row})`
                    },
                        border(index, - 1) && line({ x1: 0, y1: 0, x2: 0, y2: 50, stroke: '#000', 'stroke-width': 2 }),
                        border(index, 1) && line({ x1: 50, y1: 0, x2: 50, y2: 50, stroke: '#000', 'stroke-width': 2 }),
                        border(index, -state.columns) && line({ x1: 0, y1: 0, x2: 50, y2: 0, stroke: '#000', 'stroke-width': 2 }),
                        border(index, state.columns) && line({ x1: 0, y1: 50, x2: 50, y2: 50, stroke: '#000', 'stroke-width': 2 }),
                    )
                ),
            -}
            <> [maybeN $ (if length (sinks state) < state^._nbSinks then sinkCursor else tileCursor) <$> state^._pointerPosition]
    ]

    board = incDecGrid lens state [grid]

    rules = [text "blah blah"]
    winTitle = "GAGNÉ"
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


    const border = (i, j) => state.position[i] > 0 && state.position[i] !== state.position[i + j];

    const Grid = () =>
        C.Board({
            trackPointer: true,
            class: 'ui-board',
            style: gridStyle(state.rows, state.columns),
            oncontextmenu: combine(actions.rotate, preventDefault),
        },
            svg({ viewBox: `0 0 ${50 * state.columns} ${50 * state.rows}`, width: '100%', height: '100%' },
                repeat2(state.rows, state.columns, (row, col, index) =>
                    Square({
                        isBlack: state.coloringVisible && (row + col) % 2 === 0,
                        hasBlock: state.position[index] > 0,
                        hasSink: state.position[index] === -1,
                        row,
                        col,
                        borders: [-1, 1, -state.columns, state.columns].map(i =>
                            state.position[index] > 0 && state.position[index] !== state.position[index + i]
                        ),
                        onclick: [actions.clickOnCell, index],
                        onpointerenter: [actions.setHoverSquare, index],
                        onpointerleave: [actions.setHoverSquare, null]
                    })
                ),
                repeat2(state.rows, state.columns, (row, col, index) =>
                    g({
                        transform: `translate(${50 * col}, ${50 * row})`
                    },
                        border(index, - 1) && line({ x1: 0, y1: 0, x2: 0, y2: 50, stroke: '#000', 'stroke-width': 2 }),
                        border(index, 1) && line({ x1: 50, y1: 0, x2: 50, y2: 50, stroke: '#000', 'stroke-width': 2 }),
                        border(index, -state.columns) && line({ x1: 0, y1: 0, x2: 50, y2: 0, stroke: '#000', 'stroke-width': 2 }),
                        border(index, state.columns) && line({ x1: 0, y1: 50, x2: 50, y2: 50, stroke: '#000', 'stroke-width': 2 }),
                    )
                ),
                state.pointerPosition && (state.sinks.length < state.nbSinks ? SinkCursor() : TileCursor())
            )
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

    const CustomTileDialog = () =>
        Dialog({
            title: 'Succès',
            onOk: [actions.showDialog, null]
        },
            div({ class: 'ui-flex-center tiling-success-container' },
                Dialog({
                    onOk: [actions.showDialog, null],
                    title: 'Tuile personnalisée'
                },
                    div({ class: 'tiling-customtile-grid-container' },
                        div({
                            class: 'tiling-grid',
                            style: gridStyle(5, 5)
                        },
                            svg({ viewBox: '0 0 250 250', width: '100%', height: '100%' },
                                repeat2(5, 5, (row, col, index) =>
                                    Square({
                                        key: index,
                                        row,
                                        col,
                                        hasBlock: state.customTile[index],
                                        onclick: [actions.flipTile, index]
                                    })
                                )
                            )
                        )

                    )
                )
            )
        );

    const message =
        state.sinks.length === 0 && state.nbSinks > 0
            ? 'Place le premier évier'
            : state.sinks.length === 1 && state.nbSinks > 1
                ? 'Place le second évier'
                : 'Partie commencée';

    const Board = () =>
        div(
            C.IncDecGrid(Grid()),
            span({ class: 'tiling-message' }, message)
        );

    const dialogs = {
        success: SuccessDialog,
        customtile: CustomTileDialog,
    };

});