module Game.Paths.View (view) where
import MyPrelude
import Lib.Util (coords, tabulate)
import Game.Core (PointerPosition, _nbRows, _nbColumns, _position, _help, _pointer)
import Game.Effs (EFFS)
import Game.Paths.Model (State, Mode(..), _exit, _mode, selectVertexA, selectModeA)
import Pha (VDom, Prop, text, emptyNode, maybeN, ifN)
import Pha.Html (div, p, br, key, class', attr, onclick, style, pc)
import Pha.Svg (svg, g, path, use, viewBox)
import Pha.Util (translate)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, iconSelectGroup, ihelp, iundo, iredo, ireset, irules)
import UI.Template (template, card, incDecGrid, gridStyle, svgCursorStyle, trackPointer)

square :: ∀a. {darken :: Boolean, trap :: Boolean, door :: Boolean, x :: Number, y :: Number} -> Array (Prop a EFFS) -> VDom a EFFS
square {darken, trap, door, x, y} props =
    g ([class' "paths-darken" darken] <> props) [
        use x y 100.0 100.0 "#paths-background" [],
        ifN door \_ ->
            use x y 100.0 100.0 "#paths-door" [],
        use x y 100.0 100.0 "#paths-trap" [class' "paths-trap" true, class' "visible" $ trap && not door]
    ]

doorCursor :: ∀a. PointerPosition -> VDom a EFFS
doorCursor pp =
    use (-50.0) (-50.0) 100.0 100.0 " #paths-door" $ [
        key "cdoor",
        attr "opacity" "0.6",
        attr "pointer-events" "none"
    ] <> svgCursorStyle pp
        
heroCursor :: ∀a. PointerPosition -> VDom a EFFS
heroCursor pp =
    use (-40.0) (-40.0) 80.0 80.0 " #meeplehat" $ [
        key "chero",
        attr "opacity" "0.6",
        attr "pointer-events" "none"
    ] <> svgCursorStyle pp

view :: State -> VDom State EFFS
view state = template _{config=config, board=board, rules=rules} state where
    position = state^._position
    rows = state^._nbRows
    columns = state^._nbColumns
    
    config = card "Chemins" [
        iconSelectGroup state "Mode de jeu" [Mode1, Mode2] (state^._mode) selectModeA case _ of
            Mode1 -> _{icon = IconSymbol "#paths-mode0", tooltip = Just "Mode 1"}
            Mode2 -> _{icon = IconSymbol "#paths-mode1", tooltip = Just "Mode 2"},
        iconSizesGroup state [4∧6, 5∧5, 3∧8] true,
        icongroup "Options" $ [ihelp, iundo, iredo, ireset, irules] <#> \x -> x state
    ]

    hero = 
        last position <#> \h ->
            let {row, col} = coords columns h in
            use 0.0 0.0 80.0 80.0 "#meeplehat" [
                key "hero",
                class' "paths-hero" true,
                style "transform" $ translate (pc $ (toNumber col + 0.1) / toNumber columns)
                                              (pc $ (toNumber row + 0.1) / toNumber rows)
            ]

    pathdec = joinWith " " $ concat $ position # mapWithIndex \i v ->
        let {row, col} = coords columns v in [if i == 0 then "M" else "L", show $ 100 * col + 50, show $ 100 * row + 50]
    
    grid = div (gridStyle rows columns 5 <> trackPointer) [
        svg [viewBox 0 0 (100 * columns) (100 * rows)] $
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
                    onclick $ selectVertexA index
                ]
            ) <> [
                path pathdec [class' "paths-path" true],
                maybeN hero,
                maybeN $ state^._pointer <#> \pp ->
                    if null position then
                        heroCursor pp
                    else if isNothing $ state^._exit then
                        doorCursor pp
                    else
                        emptyNode
            ]
    ]

    board = incDecGrid state [grid]

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
