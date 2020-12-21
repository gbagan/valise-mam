module Game.Paths.View (view) where
import MyPrelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Lib.Util (coords)
import Game.Core (PointerPosition, _nbRows, _nbColumns, _position, _help, _pointer)
import Game.Paths.Model (State, Msg(..), Mode(..), _exit, _mode)
import Pha as H
import Pha.Elements as HH
import Pha.Attributes as P
import Pha.Events as E
import Pha.Util (pc, translate)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, iconSelectGroup', ihelp, iundo, iredo, ireset, irules)
import UI.Template (template, card, incDecGrid, gridStyle, svgCursorStyle, trackPointer)

square ∷ ∀a. {darken ∷ Boolean, trap ∷ Boolean, door ∷ Boolean, x ∷ Number, y ∷ Number} → Array (H.Prop a) → H.VDom a
square {darken, trap, door, x, y} props =
    HH.g ([H.class' "paths-darken" darken] <> props)
    [   HH.use ([P.href "#paths-background"] <> pos)
    ,   H.when door \_ →
            HH.use ([P.href "#paths-door"] <> pos)
    ,   HH.use (pos <>
        [   P.href "#paths-trap" 
        ,   H.class_ "paths-trap"
        ,   H.class' "visible" (trap && not door)
        ])
    ]
    where pos = [P.x x, P.y y, P.width "100", P.height "100"]

doorCursor ∷ ∀a. PointerPosition → H.VDom a
doorCursor pp =
    HH.use $
    [   P.href "#paths-door" 
    ,   H.key "cdoor"
    ,   H.class_ "paths-cursor"
    ,   P.x (-50.0)
    ,   P.y (-50.0)
    ,   P.width "100"
    ,   P.height "100"
    ] <> svgCursorStyle pp
        
heroCursor ∷ ∀a. PointerPosition → H.VDom a
heroCursor pp =
    HH.use $
    [   P.href "#meeplehat"
    ,   H.key "chero"
    ,   H.class_ "paths-cursor"
    ,   P.x (-40.0)
    ,   P.y (-40.0)
    ,   P.width "80"
    ,   P.height "80"
    ] <> svgCursorStyle pp

view ∷ State → H.VDom Msg
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
        [   iconSelectGroup' state "Mode de jeu" mode SelectMode
            [   Mode1 /\ _{icon = IconSymbol "#paths-mode0", tooltip = Just "Mode 1"}
            ,   Mode2 /\ _{icon = IconSymbol "#paths-mode1", tooltip = Just "Mode 2"}
            ]
        ,   iconSizesGroup state [4∧6, 5∧5, 3∧8] true
        ,   icongroup "Options" $ [ihelp, iundo, iredo, ireset, irules] <#> (_ $ state)
        ]

    hero h = 
        let {row, col} = coords columns h in
        HH.use 
        [   P.href "#meeplehat"
        ,   H.key "hero"
        ,   P.width "80"
        ,   P.height "80"
        ,   H.class_ "paths-hero"
        ,   H.style "transform" $ translate (pc $ (toNumber col + 0.1) / toNumber columns)
                                          (pc $ (toNumber row + 0.1) / toNumber rows)
        ]

    pathdec ∷ String
    pathdec = joinWith " " $ position # foldMapWithIndex \i v →
        let {row, col} = coords columns v in
        [if i == 0 then "M" else "L", show $ 100 * col + 50, show $ 100 * row + 50]
    
    grid = HH.div (gridStyle rows columns 5 <> trackPointer) [
        HH.svg [P.viewBox 0 0 (100 * columns) (100 * rows)] $
            (repeat (rows * columns) \index →
                let {row, col} = coords columns index in
                square
                {   darken: help && even (row + col)
                ,   trap: elem index position && Just index ≠ last position
                ,   door: exit == Just index
                ,   x: toNumber (100 * col)
                ,   y: toNumber (100 * row)
                }
                [   H.key $ show index
                ,   E.onclick $ SelectVertex index
                ]
            ) <>
            [   HH.path [P.d pathdec, H.class_ "paths-path"]
            ,   H.maybe (last position) hero
            ,   H.maybe pointer \pp →
                    if null position then
                        heroCursor pp
                    else if isNothing exit then
                        doorCursor pp
                    else
                        H.emptyNode
            ]
    ]

    board = incDecGrid state [grid]

    rules = [
        HH.p [] 
        [   H.text "Après moultes péripéties dans le temple maudit de Berge, le professeur Hamilton Jones se retrouve dans la dernière salle"
        ,   HH.br
        ,   H.text "Pour sortir de celle-ci, il doit s\'enfuir par une porte au-dessous de lui."
        ,   HH.br
        ,   H.text "Celle-ci ne peut être ouverte qu\'en marchant sur chacune des dalles dans la salle."
        ],
        HH.p [] 
        [   H.text "Malheusement, ces dalles sont piégées, le piège se déclenchant peu de temps après avoir marché dessus."
        ,   HH.br
        ,   H.text "Donc, Hamilton ne peut pas remarcher sur une dalle sur laquelle il a déjà été."
        ,   HH.br,
            H.text "N'ayant plus l'aisance de sa jeunesse, Hamilton ne peut se déplacer que d'une dalle à la fois et ne peut le faire en diagonale."
        ],
        HH.p []
        [   H.text "Trouve un parcours pour résoudre l\'énigme. Ca semble facile ? Mais, cela est-il possible pour toutes les tailles de grille ?"],
        HH.p [] 
        [   H.text "Dans le deuxième mode de jeu, tu peux choisir la position de départ d\'Hamilton ainsi que celle de la porte."
        ,   HH.br
        ,   H.text "Tu remarqueras qu\'il n\'y a pas toujours de solution."
        ,   HH.br,
            H.text "Trouve des critères sur les positions d\'Hamilton et de la porte pour qu\'une solution soit possible."
        ]
    ]
