module Game.Paths.View where
import Prelude
import Data.Lens (Lens', (^.))
import Data.Tuple (Tuple (..))
import Data.Array (concat, elem, last, mapWithIndex, null)
import Data.Maybe (Maybe(..), maybe)
import Data.Int (toNumber, even)
import Data.String (joinWith)
import Lib.Core (coords, tabulate)
import Pha.Class (VDom, Prop)
import Game.Core (_nbRows, _nbColumns, _position, _help)
import Game.Paths.Model (PathsState, _exit, selectVertexA)
import Pha (emptyNode, text)
import Pha.Action ((🎲))
import Pha.Html (div', p, br, g, svg, use, path, key, class', click, style, width, height, viewBox)
import UI.Dialog (card)
import UI.Icon (icongroup)
import UI.Icons (iconSizesGroup, ihelp, iundo, iredo, ireset, irules)
import UI.Template (template, incDecGrid, gridStyle)

square :: forall a. Boolean -> Boolean -> Boolean -> Number -> Number -> Array (Prop a) -> VDom a
square darken trap  door x y props =
    g ([class' "paths-darken" darken] <> props) [
        use x y 100.0 100.0 "#paths-background" [],
        if door then use x y 100.0 100.0 "#paths-door" [] else emptyNode,
        use x y 100.0 100.0 "#paths-trap" [class' "paths-trap" true, class' "visible" $ trap && not door]
    ]

view :: forall a. Lens' a PathsState -> PathsState -> VDom a
view lens state = template lens {config, board, rules, winTitle} state where
    position = state^._position
    rows = state^._nbRows
    columns = state^._nbColumns
    
    config = card "Chemins" [
           {- I.Group({
                title: 'Mode de jeu',
                list: [0, 1],
                symbol: 'paths-mode',
                select: state.mode,
                tooltip: ['Mode 1', 'Mode 2'],
                onclick: actions.selectMode
            }),
            -}

        iconSizesGroup lens state [Tuple 4 6, Tuple 5 5, Tuple 3 8] true, -- custom
        icongroup "Options" $ [ihelp, iundo, iredo, ireset, irules] <#> \x -> x lens state
    ]

    hero = 
        last position # maybe emptyNode \h ->
            let {row, col} = coords columns h in
            use 0.0 0.0 80.0 80.0 "#meeplehat" [
                key "hero",
                class' "paths-hero" true,
                style "transform" $ "translate(" <> show ((toNumber col * 100.0 + 10.0) / toNumber columns) <> "%,"
                                    <> show ((toNumber row * 100.0 + 10.0) / toNumber rows) <> "%)"
            ]

    {-
    doorCursor = () =>
        use({
            href: '#paths-door',
            x: -50,
            y: -50,
            width: 100,
            height: 100,
            opacity: 0.6,
            'pointer-events': 'none',
            style: svgCursorStyle(state.pointerPosition)
        });

    const HeroCursor = () =>
        use({
            href: '#meeplehat',
            x: -40,
            y: -40,
            width: 80,
            height: 80,
            opacity: 0.6,
            'pointer-events': 'none',
            style: svgCursorStyle(state.pointerPosition)
        });
    -}

    pathdec = joinWith " " $ concat $ position # mapWithIndex \i v ->
        let {row, col} = coords columns v in [if i == 0 then "M" else "L", show $ 100 * col + 50, show $ 100 * row + 50]
    
    grid = div' (gridStyle rows columns)  [
        -- trackPointer: true,
        svg [width "100%", height "100%", viewBox $ "0 0 " <> show (100 * columns) <> " " <> show (100 * rows)] $
            (tabulate (rows * columns) \index ->
                let {row, col} = coords columns index in
                square
                    (state^._help && even (row + col))  -- darken
                    (elem index position && Just index /= last position)  -- trap
                    (state^._exit == Just index)     --- door
                    (toNumber $ 100 * col)
                    (toNumber $ 100 * row) [
                    key $ show index,
                    click $ lens 🎲 selectVertexA index
                ]
            ) <> [
                path pathdec [class' "paths-path" true],
                hero
                {-
                if null position then
                    state.pointerPosition && heroCursor [key "chero"]
                else if isNothing state.exit then
                    state.pointerPosition && doorCursor [key "cdoor"]
                else emptyNode
                -}
            ]
    ]

    board = incDecGrid lens state [grid]

    rules = [
        p [] [
            text "Après de moultes péripéties dans le temple maudit de Berge, le professeur Hamilton Jones se retrouve dans la dernière salle", br,
            text "Pour sortir de celle-ci, il doit s\'enfuir par une porte au dessous de lui.", br,
            text "Celle ci ne peut être ouverte qu\'en marchant sur chacune des dalles dans la salle."
        ],
        p [] [
            text "Malheusement, ces dalles sont piégées, le piège se déclenchant peu de temps après avoir marché dessus.", br,
            text "Donc, Hamilton ne peut pas remarcher sur une dalle sur laquelle il a déjà été.", br,
            text "N'ayant plus l'aisance de sa jeunesse, Hamilton ne peut se déplacer que d'une dalle à la fois et ne peut le faire en diagonale."
        ],
        p [] [text "Trouve un parcours pour résoudre l\'énigme. Ca semble facile? Mais, cela est-il possible pour toutes les tailles de grille"],
        
        p [] [
            text "Dans le deuxième mode de jeu, tu peux choisir la position de départ d\'Hamilton ainsi que celle de la porte.", br,
            text "Tu remarqueras qu\'il n\'y a pas toujours de solution.", br,
            text "Trouve des critères sur les positions d\'Hamilton et de la porte pour qu\'une solution soit possible."
        ]
    ]

    winTitle = "GAGNÉ"