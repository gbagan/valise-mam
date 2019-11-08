module Game.Labete.View where

import MyPrelude
import Pha (VDom, Prop, text, whenN, maybeN)
import Pha.Action ((🔍))
import Pha.Html (div', span, br, svg, g, rect, use, key, attr, class', style, click, pointerdown, pointerup, pointerleave,
                fill, stroke, strokeWidth, transform, viewBox, width, height)
import Game.Core (_position, _nbColumns, _nbRows, _pointer, _help, playA)
import Game.Effs (EFFS)
import Game.Labete.Model (State, Mode(..), _mode, _beastIndex, nonTrappedBeastOnGrid, setModeA, setHelpA, setBeastA)
import Lib.Util (coords, map2)
import UI.Template (template, card, incDecGrid, gridStyle, trackPointer, svgCursorStyle)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSelectGroup, iconSizesGroup, ireset, irules)

colors :: Array String
colors = ["#5aa02c", "blue", "red", "yellow", "magenta", "cyan", "orange", "darkgreen", "grey"]

{-
const Zone = ({ x1, y1, x2, y2, color }) =>
    rect({
        stroke: 'black',
        fill: colors[color],
        'pointer-events': 'none',
        opacity: 0.4,
        x: Math.min(x1, x2) + '%',
        y: Math.min(y1, y2) + '%',
        width: Math.abs(x2 - x1) + '%',
        height: Math.abs(y2 - y1) + '%'
    });
-}

modes :: Array Mode
modes = [StandardMode, CylinderMode, TorusMode]

square :: ∀a effs. { hasTrap :: Boolean, hasBeast :: Boolean, row :: Int, col :: Int} -> Array (Prop a effs) -> VDom a effs
square { hasTrap, hasBeast, row, col } props =
    g ([transform $ "translate(" <> show (50 * col) <> " " <> show(50 * row) <> ")"] <> props) [
        use 0.0 0.0 50.0 50.0 "#grass" [fill "#5aa02c"], -- fromMaybe "blackcolors[color] }),
        rect 0.0 0.0 51.0 51.0 [stroke "black", strokeWidth "0.5", fill "transparent"],
        use 5.0 5.0 40.0 40.0 "#paw" [class' "labete-beast" true, class' "visible" hasBeast],
        whenN hasTrap \_ ->
            use 5.0 5.0 40.0 40.0 "#trap" []
    ]

ihelp :: ∀a. Lens' a State -> State -> VDom a EFFS
ihelp lens state =
    iconbutton
        state
        (_{icon = IconSymbol "#help", tooltip = Just "Aide", selected = state^._help}) [
            pointerdown $ lens 🔍 setHelpA true,
            pointerup $ lens 🔍 setHelpA false,
            pointerleave $ lens 🔍 setHelpA false
        ]

view :: ∀a. Lens' a State -> State -> VDom a EFFS
view lens state = template lens (_{config = config, board = board, rules = rules, winTitle = winTitle}) state where
    rows = state^._nbRows
    columns = state^._nbColumns
    nonTrappedBeast = nonTrappedBeastOnGrid state
    {-
    const BeastView = () => (
        <svg width={50*getBeast(state).length + 2} height="52" fill="yellow" stroke="black" strokeWidth="1">
            {getBeast(state).map((b, j) => b.map(({row, col}, i) =>
                <rect key={j* 25 + i} x={j * 50 + col*10+21} y={row*10+21} width="10" height="10" />   
            ))}
        </svg>
    );
    -}

    config = card "La bête" [
        iconSelectGroup lens state "Forme de la bête" [0, 1, 2, 3] (state^._beastIndex) setBeastA \i opt -> case i of
            0 -> opt{icon = IconSymbol "#beast1"}
            1 -> opt{icon = IconSymbol "#beast2"}
            2 -> opt{icon = IconSymbol "#beast3"}
            _ -> opt{icon = IconSymbol "#beast23"},
        iconSelectGroup lens state "Type de la grille" modes (state^._mode) setModeA \i opt -> case i of
            StandardMode -> opt{icon = IconSymbol "#grid-normal", tooltip = Just "Normale"}
            CylinderMode -> opt{icon = IconSymbol "#grid-cylinder", tooltip = Just "Cylindrique"}
            TorusMode -> opt{icon = IconSymbol "#grid-torus", tooltip = Just "Torique"},

        iconSizesGroup lens state [3~3, 5~5, 6~6] true,

        icongroup "Options" $ [ihelp, ireset, irules] <#> \x -> x lens state 
        {-    I.Group({ title: `Meilleur score (${state.bestScore || 'Ø'})` },
                I.BestScore()
            )
        )
        -}
    ]

    rules = [
        text "Place le moins de pièges possible pour empécher la bête d\'abimer ta belle pelouse!", br,
        text "Tu peux choisir de jouer avec des bêtes de différentes formes comme celles préfinies dans 'Forme de la bête'", br,
        text "Dans le dernier choix, la bête peut prendre soit une ou l\'autre des formes indiquées.", br,
        text "Le plateau de jeu peut prendre une grille, un cylindre ou un tore"
    ]

    cursor pp = use (-20.0) (-20.0) 40.0 40.0 "#trap" (svgCursorStyle pp <> [  
        key "cursor",
        attr "opacity" "0.7", -- state.position[state.squareHover] ? 0.3 : 0.7,
        attr "pointer-events" "none"
    ])

    grid = div' (gridStyle rows columns 5 <> trackPointer lens <> [class' "ui-board" true]) [
           -- onpointerdown: actions.when(shift, actions.startZone2),
        svg [viewBox 0 0 (50 * columns) (50 * rows), width "100%", height "100%"] $
            (map2 (state^._position) nonTrappedBeast \index hasTrap hasBeast ->
                let {row, col} = coords columns index in
                square { row, col, hasTrap, hasBeast: hasBeast && state^._help } [
                    key $ show index,
                    -- color: state.squareColors[index],
                    -- hasBeast: state.beastVisible && state.nonTrappedBeast && state.nonTrappedBeast[index],
                    click $ lens 🔍 playA index --  actions.unless(orF(shift, attr('zoneStart')), [actions.play, index]),
                    --    onpointerenter: [actions.setSquareHover, index],
                    --    onponterleave: [actions.setSquareHover, null],
                    --    onpointerup: [actions.finishZone, index],
                    --    onpointerdown: actions.when(shift, [actions.startZone, index])
                ]
                -- state.zone && Zone({ key: 'zone', ...state.zone }),
            ) <> [maybeN $ cursor <$> state^._pointer]   --  && !state.zoneStart && Cursor({ key: 'cursor' })
    ]

    board = incDecGrid lens state [grid]
    winTitle = "GAGNÉ"

           {- state.selectedColor > 0 && div({
                class: 'labete-color',
                style: { 'background-color': colors[state.selectedColor] },
            })
        );
        -}

    {-
    const BestScoreDialog = () =>
        Dialog({
            title: 'Meilleur score',
            onOk: [actions.showDialog, null],
        },
            div({ 
                class: 'ui-board ui-flex-center labete-bestscore-grid-container',
                style: gridStyle(state.rows, state.columns)
            },
                svg({ viewBox: `0 0 ${50 * state.columns} ${50 * state.rows}`, width: '100%', height: '100%' },
                    repeat2(state.rows, state.columns, (row, col, index) =>
                        Square({
                            key: index,
                            row,
                            col,
                            color: 0,
                            hasTrap: state.bestPosition[index]
                        })
                    )
                )
            )
        )

const CustomBeastDialog = () =>
    Dialog({
        onOk: [actions.showDialog, null],
        title: 'Bête personnalisée'
    },
        div({ class: 'labete-custombeast-grid-container' },
            svg({ viewBox: '0 0 250 250', width: '100%', height: '100%' },
                repeat2(5, 5, (row, col, index) =>
                    Square({
                        key: index,
                        row,
                        col,
                        color: 0,
                        hasBeast: state.customBeast[index],
                        onclick: [actions.flipBeast, index]
                    })
                )
            )
        )
    );

const dialogs = {
    bestscore: BestScoreDialog,
    custombeast: CustomBeastDialog,
};

return {
    HelpDialog,
    Board,
    Config,
    dialogs,
    winTitle: `Record: ${sum(state.position)} pièges`,
};
});