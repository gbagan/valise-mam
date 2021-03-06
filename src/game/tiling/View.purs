module Game.Tiling.View (view) where
import MyPrelude
import Lib.Util (coords)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Keyed as K
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (translate)
import Game.Common (_isoCustom)
import Game.Core (_position, _nbRows, _nbColumns, _pointer, _help)
import Game.Tiling.Model (State, Msg(..), TileType(..), _nbSinks, _rotation, _tile, _tileType,
                          sinks, needSinks, inConflict)
import UI.Template (template, card, dialog, incDecGrid, gridStyle, svgCursorStyle, trackPointer)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, iconSelectGroup, ihelp, ireset, irules)

type Borders = {top ∷ Boolean, left ∷ Boolean, bottom ∷ Boolean, right ∷ Boolean}

square ∷ ∀a. {isDark ∷ Boolean, hasBlock ∷ Boolean, hasSink ∷ Boolean, row ∷ Int, col ∷ Int} → Array (H.Prop a) → Html a 
square {isDark, hasBlock, hasSink, row, col} props =
    H.g (
    [   H.class' "tiling-darken" isDark
    ,   P.transform $ translate (show $ 50 * col) (show $ 50 * row)
    ] <> props) 
    [   H.rect [P.width "50", P.height "50", P.fill "url(#concrete)"]
    ,   H.when hasBlock \_ →
            H.use [P.href "#tile2", P.width "50", P.height "50"]
    ,   H.when hasSink \_ →
            H.use [P.href "#sink", P.width "50", P.height "50"]
    ]
    
view ∷ State → Html Msg
view state = template {config, board, rules, winTitle, customDialog} state where
    position = state ^. _position
    rows = state ^. _nbRows
    columns = state ^. _nbColumns
    tileType = state ^. _tileType
    nbSinks = state ^. _nbSinks
    tile = state ^. _tile
    rotation = state ^. _rotation
    help = state ^. _help
    pointer = state ^. _pointer
    
    border i di = position !! i ≠ position !! (i + di)

    config =
        card "Carrelage"
        [   iconSizesGroup state [4∧5, 5∧5, 5∧6, 8∧8] true
        ,   iconSelectGroup state "Motif de la tuile" [Type1, Type2, Type3, CustomTile] tileType SetTile \t →
                _{icon = IconSymbol ("#" <> show t)}
        ,   iconSelectGroup state "Nombre d'éviers" [0, 1, 2] nbSinks SetNbSinks (const identity)
        ,   icongroup "Options" $ [ihelp, ireset, irules] <#> (_ $ state)
        ]

    tileCursor pp =
        H.g (svgCursorStyle pp)
        [   H.g [
                H.class_ "tiling-cursor",
                H.style "transform" $ "rotate(" <> show (90 * rotation) <> "deg)"
            ] $ tile <#> \{row, col} →
                H.use
                [   P.href "#tile2"
                ,   P.x $ 50.0 * toNumber col - 25.0
                ,   P.y $ 50.0 * toNumber row - 25.0
                ,   P.width "50"
                ,   P.height "50"
                ,   H.attr "pointer-events" "none"
                ,   P.opacity (if inConflict state then 0.3 else 0.8)
                ]
        ]
        
    sinkCursor pp =
        H.use ([
            P.href "#sink", P.x (-25.0), P.y (-25.0), P.width "50", P.height "50",
            H.attr "pointer-events" "none"
        ] <> svgCursorStyle pp)

    grid =
        H.div (gridStyle rows columns 5 <> trackPointer <> [
            H.class_ "ui-board",
            E.onContextMenu Rotate
        ]) [
            H.svg [P.viewBox 0 0 (50 * columns) (50 * rows)] $ concat
            [   position # mapWithIndex \index pos →
                    let {row, col} = coords columns index in
                    square
                    {   isDark: help && even (row + col)
                    ,   hasBlock: pos > 0
                    ,   hasSink: pos == -1
                    ,   row
                    ,   col
                    }
                    [   E.onClick $ if needSinks state then PutSink index else Play index
                    ,   E.onPointerEnter $ SetHoverSquare (Just index)
                    ,   E.onPointerLeave $ SetHoverSquare Nothing
                    ]
            ,   position # mapWithIndex \index pos →
                    let {row, col} = coords columns index in
                    H.g [P.transform $ translate (show $ 50 * col) (show $ 50 * row)]
                    [   H.when (pos > 0 && border index (-1)) \_ →
                            H.line [P.x1 0.0, P.y1 0.0, P.x2 0.0, P.y2 50.0, P.stroke "#000", P.strokeWidth 2.0]
                    ,   H.when (pos > 0 && border index 1) \_ →
                            H.line [P.x1 50.0, P.y1 0.0, P.x2 50.0, P.y2 50.0, P.stroke "#000", P.strokeWidth 2.0]
                    ,   H.when (pos > 0 && border index (-columns)) \_ →
                            H.line [P.x1 0.0, P.y1 0.0, P.x2 50.0, P.y2 0.0, P.stroke "#000", P.strokeWidth 2.0]
                    ,   H.when (pos > 0 && border index columns) \_ →
                            H.line [P.x1 0.0, P.y1 50.0, P.x2 50.0, P.y2 50.0, P.stroke "#000", P.strokeWidth 2.0]    
                    ]
            ,   [H.maybe pointer $ if length (sinks state) < nbSinks then sinkCursor else tileCursor]
            ]
        ]

    board = incDecGrid state [grid]

    customDialog _ = dialog "Personnalise ta tuile" [
        H.div [H.class_ "tiling-customtile-grid-container"] [
            H.div [H.class_ "tiling-grid"] [
                K.svg [P.viewBox 0 0 250 250] (
                    tile ^. _isoCustom # mapWithIndex \index hasBlock →
                        let {row, col} = coords 5 index
                        in show index /\ square {hasBlock, row, col, hasSink: false, isDark: false}
                            [E.onClick $ FlipTile index]
                )
            ]
        ]
    ]

    rules =
        [   H.text "Est-il possible de faire le carrelage de toute votre cuisine, sachant qu'elle peut avoir un ou plusieurs éviers ?"
        ,   H.br
        ,   H.text "Tu peux tester avec différentes formes de tuile et différents emplacements d'éviers."
        ,   H.br
        ,   H.text "Deux questions sont particulièrement intéressantes:"
        ,   H.br
        ,   H.text "- Pour quelles dimensions de la grille et pour quels positions d'éviers peut-on paver une grille avec le premier type de tuiles?"
        ,   H.br
        ,   H.text "- Peut-on toujours carreler une grille 8x8 avec les tuiles de type 3 et en posant un évier, et ceci, quelque soit la position de l'évier?"
        ]

    winTitle = "GAGNÉ"

        {-
        I.Icon({
                    symbol: 'cup',
                    tooltip: 'Succès',
                    onClick: [actions.showDialog, 'success']

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
