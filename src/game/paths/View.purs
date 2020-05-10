module Game.Paths.View (view) where
import MyPrelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Lib.Util (coords, repeat)
import Game.Core (PointerPosition, _nbRows, _nbColumns, _position, _help, _pointer)
import Game.Paths.Model (State, Msg(..), Mode(..), _exit, _mode)
import Pha (VDom, Prop, text, emptyNode, (<&&>), (<??>), class_, class', key, style)
import Pha.Elements (div, p, br)
import Pha.Attributes (href)
import Pha.Events (onclick)
import Pha.Svg (svg, g, path, use, viewBox, d_, x_, y_, width, height)
import Pha.Util (pc, translate)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, iconSelectGroup, ihelp, iundo, iredo, ireset, irules)
import UI.Template (template, card, incDecGrid, gridStyle, svgCursorStyle, trackPointer)

square ∷ ∀a. {darken ∷ Boolean, trap ∷ Boolean, door ∷ Boolean, x ∷ Number, y ∷ Number} → Array (Prop a) → VDom a
square {darken, trap, door, x, y} props =
    g ([class' "paths-darken" darken] <> props)
    [   use ([href "#paths-background"] <> pos)
    ,   door <&&> \_ →
            use ([href "#paths-door"] <> pos)
    ,   use (pos <> 
        [   href "#paths-trap" 
        ,   class_ "paths-trap"
        ,   class' "visible" (trap && not door)
        ])
    ]
    where pos = [x_ $ show x, y_ $ show y, width "100", height "100"]

doorCursor ∷ ∀a. PointerPosition → VDom a
doorCursor pp =
    use $
    [   href "#paths-door" 
    ,   key "cdoor"
    ,   class_ "paths-cursor"
    ,   x_ "-50"
    ,   y_ "-50"
    ,   width "100"
    ,   height "100"
    ] <> svgCursorStyle pp
        
heroCursor ∷ ∀a. PointerPosition → VDom a
heroCursor pp =
    use $
    [   href "#meeplehat"
    ,    key "chero"
    ,   class_ "paths-cursor"
    ,   x_ "-40"
    ,   y_ "-40"
    ,   width "80"
    ,   height "80"
    ] <> svgCursorStyle pp

view ∷ State → VDom Msg
view state = template {config, board, rules} state where
    position = state ^. _position
    rows = state ^. _nbRows
    columns = state ^. _nbColumns
    exit = state ^. _exit
    mode = state ^. _mode
    help = state ^. _help
    pointer = state ^. _pointer

    config =
        card "Chemins"
        [   iconSelectGroup state "Mode de jeu" [Mode1, Mode2] mode SelectMode case _ of
                Mode1 → _{icon = IconSymbol "#paths-mode0", tooltip = Just "Mode 1"}
                Mode2 → _{icon = IconSymbol "#paths-mode1", tooltip = Just "Mode 2"}
        ,   iconSizesGroup state [4∧6, 5∧5, 3∧8] true
        ,   icongroup "Options" $ [ihelp, iundo, iredo, ireset, irules] <#> (_ $ state)
        ]

    hero h = 
        let {row, col} = coords columns h in
        use 
        [   href "#meeplehat"
        ,   key "hero"
        ,   width "80"
        ,   height "80"
        ,   class_ "paths-hero"
        ,   style "transform" $ translate (pc $ (toNumber col + 0.1) / toNumber columns)
                                          (pc $ (toNumber row + 0.1) / toNumber rows)
        ]

    pathdec = joinWith " " $ position # foldMapWithIndex \i v →
        let {row, col} = coords columns v in
        [if i == 0 then "M" else "L", show $ 100 * col + 50, show $ 100 * row + 50]
    
    grid = div (gridStyle rows columns 5 <> trackPointer) [
        svg [viewBox 0 0 (100 * columns) (100 * rows)] $
            (repeat (rows * columns) \index →
                let {row, col} = coords columns index in
                square
                {   darken: help && even (row + col)
                ,   trap: elem index position && Just index /= last position
                ,   door: exit == Just index
                ,   x: toNumber (100 * col)
                ,   y: toNumber (100 * row)
                }
                [   key $ show index
                ,   onclick $ SelectVertex index
                ]
            ) <>
            [   path [d_ pathdec, class_ "paths-path"]
            ,   last position <??> hero,
                    pointer <??> \pp →
                        if null position then
                            heroCursor pp
                        else if isNothing exit then
                            doorCursor pp
                        else
                            emptyNode
            ]
    ]

    board = incDecGrid state [grid]

    rules = [
        p [] [
            text "Après moultes péripéties dans le temple maudit de Berge, le professeur Hamilton Jones se retrouve dans la dernière salle", br,
            text "Pour sortir de celle-ci, il doit s\'enfuir par une porte au-dessous de lui.", br,
            text "Celle-ci ne peut être ouverte qu\'en marchant sur chacune des dalles dans la salle."
        ],
        p [] [
            text "Malheusement, ces dalles sont piégées, le piège se déclenchant peu de temps après avoir marché dessus.", br,
            text "Donc, Hamilton ne peut pas remarcher sur une dalle sur laquelle il a déjà été.", br,
            text "N'ayant plus l'aisance de sa jeunesse, Hamilton ne peut se déplacer que d'une dalle à la fois et ne peut le faire en diagonale."
        ],
        p [] [text "Trouve un parcours pour résoudre l\'énigme. Ca semble facile ? Mais, cela est-il possible pour toutes les tailles de grille ?"],
        
        p [] [
            text "Dans le deuxième mode de jeu, tu peux choisir la position de départ d\'Hamilton ainsi que celle de la porte.", br,
            text "Tu remarqueras qu\'il n\'y a pas toujours de solution.", br,
            text "Trouve des critères sur les positions d\'Hamilton et de la porte pour qu\'une solution soit possible."
        ]
    ]
