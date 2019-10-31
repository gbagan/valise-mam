module Game.Paths.View (view) where
import Prelude
import Data.Lens (Lens', (^.))
import Data.Tuple (Tuple (..))
import Data.Array (concat, elem, last, mapWithIndex, null)
import Data.Maybe (Maybe(..), isNothing)
import Data.Int (toNumber, even)
import Data.String (joinWith)
import Lib.Util (coords, tabulate)
import Game.Core (PointerPosition, _nbRows, _nbColumns, _position, _help, _pointerPosition)
import Game.Paths.Model (PathsState, Mode(..), _exit, _mode, selectVertexA, selectModeA)
import Pha (VDom, Prop, text, emptyNode, maybeN, whenN)
import Pha.Action ((🎲))
import Pha.Html (div', p, br, g, svg, use, path, key, class', attr, click, style, width, height, viewBox)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, iconSelect, ihelp, iundo, iredo, ireset, irules)
import UI.Template (template, card, incDecGrid, gridStyle, svgCursorStyle, trackPointer)

square :: forall a. {darken :: Boolean, trap :: Boolean, door :: Boolean, x :: Number, y :: Number} -> Array (Prop a) -> VDom a
square {darken, trap, door, x, y} props =
    g ([class' "paths-darken" darken] <> props) [
        use x y 100.0 100.0 "#paths-background" [],
        whenN door \_ ->
            use x y 100.0 100.0 "#paths-door" [],
        use x y 100.0 100.0 "#paths-trap" [class' "paths-trap" true, class' "visible" $ trap && not door]
    ]

doorCursor :: forall a. PointerPosition -> VDom a
doorCursor pp =
    use (-50.0) (-50.0) 100.0 100.0 " #paths-door" $ [
        key "cdoor",
        attr "opacity" "0.6",
        attr "pointer-events" "none"
    ] <> svgCursorStyle pp
        
heroCursor :: forall a. PointerPosition -> VDom a
heroCursor pp =
    use (-40.0) (-40.0) 80.0 80.0 " #meeplehat" $ [
        key "chero",
        attr "opacity" "0.6",
        attr "pointer-events" "none"
    ] <> svgCursorStyle pp

view :: forall a. Lens' a PathsState -> PathsState -> VDom a
view lens state = template lens {config, board, rules, winTitle} state where
    position = state^._position
    rows = state^._nbRows
    columns = state^._nbColumns
    
    config = card "Chemins" [
        icongroup "Mode de jeu" [
            iconSelect lens state (state^._mode) selectModeA Mode1 (_{icon = IconSymbol "#paths-mode0", tooltip = Just "Mode 1"}),
            iconSelect lens state (state^._mode) selectModeA Mode2 (_{icon = IconSymbol "#paths-mode1", tooltip = Just "Mode 2"})
        ],
        iconSizesGroup lens state [Tuple 4 6, Tuple 5 5, Tuple 3 8] true,
        icongroup "Options" $ [ihelp, iundo, iredo, ireset, irules] <#> \x -> x lens state
    ]

    hero = 
        last position <#> \h ->
            let {row, col} = coords columns h in
            use 0.0 0.0 80.0 80.0 "#meeplehat" [
                key "hero",
                class' "paths-hero" true,
                style "transform" $ "translate(" <> show ((toNumber col * 100.0 + 10.0) / toNumber columns) <> "%,"
                                    <> show ((toNumber row * 100.0 + 10.0) / toNumber rows) <> "%)"
            ]

    pathdec = joinWith " " $ concat $ position # mapWithIndex \i v ->
        let {row, col} = coords columns v in [if i == 0 then "M" else "L", show $ 100 * col + 50, show $ 100 * row + 50]
    
    grid = div' (gridStyle rows columns 5 <> trackPointer lens) [
        svg [width "100%", height "100%", viewBox $ "0 0 " <> show (100 * columns) <> " " <> show (100 * rows)] $
            (tabulate (rows * columns) \index ->
                let {row, col} = coords columns index in
                square {
                    darken: state^._help && even (row + col),
                    trap: elem index position && Just index /= last position,
                    door: state^._exit == Just index,     --- door
                    x: toNumber (100 * col),
                    y: toNumber (100 * row)
                } [
                    key $ show index,
                    click $ lens 🎲 selectVertexA index
                ]
            ) <> [
                path pathdec [class' "paths-path" true],
                maybeN hero,
                maybeN $ state^._pointerPosition <#> \pp ->
                    if null position then
                        heroCursor pp
                    else if isNothing $ state^._exit then
                        doorCursor pp
                    else
                        emptyNode
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