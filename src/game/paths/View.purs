module Game.Paths.View (view) where
import MyPrelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Lib.Util (coords)
import Game.Core (PointerPosition, _nbRows, _nbColumns, _position, _help, _pointer)
import Game.Paths.Model (State, Msg(..), Mode(..), _exit, _mode)
import Pha.Html (Html, Prop)
import Pha.Html as H
import Pha.Html.Keyed as K
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc, translate)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, iconSelectGroup', ihelp, iundo, iredo, ireset, irules)
import UI.Template (template, card, incDecGrid, gridStyle, svgCursorStyle, trackPointer)

square ∷ ∀a. {darken ∷ Boolean, trap ∷ Boolean, door ∷ Boolean, x ∷ Number, y ∷ Number} → Array (Prop a) → Html a
square {darken, trap, door, x, y} props =
    H.g ([H.class' "paths-darken" darken] <> props)
    [   H.use ([P.href "#paths-background"] <> pos)
    ,   H.when door \_ →
            H.use ([P.href "#paths-door"] <> pos)
    ,   H.use (pos <>
        [   P.href "#paths-trap" 
        ,   H.class_ "paths-trap"
        ,   H.class' "visible" (trap && not door)
        ])
    ]
    where pos = [P.x x, P.y y, P.width "100", P.height "100"]

doorCursor ∷ ∀a. PointerPosition → Html a
doorCursor pp =
    H.use $
    [   P.href "#paths-door" 
    ,   H.class_ "paths-cursor"
    ,   P.x (-50.0)
    ,   P.y (-50.0)
    ,   P.width "100"
    ,   P.height "100"
    ] <> svgCursorStyle pp
        
heroCursor ∷ ∀a. PointerPosition → Html a
heroCursor pp =
    H.use $
    [   P.href "#meeplehat"
    ,   H.class_ "paths-cursor"
    ,   P.x (-40.0)
    ,   P.y (-40.0)
    ,   P.width "80"
    ,   P.height "80"
    ] <> svgCursorStyle pp

view ∷ State → Html Msg
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
            [   Mode1 ∧ _{icon = IconSymbol "#paths-mode0", tooltip = Just "Mode 1"}
            ,   Mode2 ∧ _{icon = IconSymbol "#paths-mode1", tooltip = Just "Mode 2"}
            ]
        ,   iconSizesGroup state [4∧6, 5∧5, 3∧8] true
        ,   icongroup "Options" $ [ihelp, iundo, iredo, ireset, irules] <#> (_ $ state)
        ]

    hero h = 
        let {row, col} = coords columns h in
        H.use 
        [   P.href "#meeplehat"
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
    
    grid = H.div (gridStyle rows columns 5 <> trackPointer) [
        K.svg [P.viewBox 0 0 (100 * columns) (100 * rows)] $
            (repeat (rows * columns) \index →
                let {row, col} = coords columns index in
                show index /\ square
                {   darken: help && even (row + col)
                ,   trap: elem index position && Just index ≠ last position
                ,   door: exit == Just index
                ,   x: toNumber (100 * col)
                ,   y: toNumber (100 * row)
                }
                [   E.onClick \_ -> SelectVertex index
                ]
            ) <>
            [   "path" /\ H.path [P.d pathdec, H.class_ "paths-path"]
            ,   "hero" /\ H.maybe (last position) hero
            ,   "cur" /\ H.maybe pointer \pp →
                    if null position then
                        heroCursor pp
                    else if isNothing exit then
                        doorCursor pp
                    else
                        H.empty
            ]
    ]

    board = incDecGrid state [grid]

    rules = [
        H.p [] 
        [   H.text "Après moultes péripéties dans le temple maudit de Berge, le professeur Hamilton Jones se retrouve dans la dernière salle"
        ,   H.br
        ,   H.text "Pour sortir de celle-ci, il doit s\'enfuir par une porte au-dessous de lui."
        ,   H.br
        ,   H.text "Celle-ci ne peut être ouverte qu\'en marchant sur chacune des dalles dans la salle."
        ],
        H.p [] 
        [   H.text "Malheusement, ces dalles sont piégées, le piège se déclenchant peu de temps après avoir marché dessus."
        ,   H.br
        ,   H.text "Donc, Hamilton ne peut pas remarcher sur une dalle sur laquelle il a déjà été."
        ,   H.br,
            H.text "N'ayant plus l'aisance de sa jeunesse, Hamilton ne peut se déplacer que d'une dalle à la fois et ne peut le faire en diagonale."
        ],
        H.p []
        [   H.text "Trouve un parcours pour résoudre l\'énigme. Ca semble facile ? Mais, cela est-il possible pour toutes les tailles de grille ?"],
        H.p [] 
        [   H.text "Dans le deuxième mode de jeu, tu peux choisir la position de départ d\'Hamilton ainsi que celle de la porte."
        ,   H.br
        ,   H.text "Tu remarqueras qu\'il n\'y a pas toujours de solution."
        ,   H.br,
            H.text "Trouve des critères sur les positions d\'Hamilton et de la porte pour qu\'une solution soit possible."
        ]
    ]
