module Game.Hanoi.View (view) where

import MyPrelude

import Game.Core (_position, _pointer)
import Game.Hanoi.Model (State, Msg(..), _dragged, _nbDisks)
import Pha.Html (Html, Prop)
import Pha.Html as H
import Pha.Html.Attributes as P
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)
import UI.Template (template, card, dndBoardProps, dndItemProps)

colors ∷ Array String
colors = ["blue", "red", "green", "magenta", "orange", "gray", "cyan"]

drawTower ∷ ∀a. Html a
drawTower = H.path [P.d ""]


view ∷ State → Html Msg
view state = template {config, board, rules, winTitle} state where
    position = state ^. _position
    nbDisks = state ^. _nbDisks
    dragged = state ^. _dragged
    pointer = state ^. _pointer

    config =
        card "Tours de Hanoi"
        [   iconSelectGroup state "Nombres de disques" [3, 4, 5, 6, 7] nbDisks SetNbDisks (const identity)
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> (_ $ state)
        ]

    drawDisk ∷ Number → Number → Int → Maybe Int → Array (Prop Msg) → Html Msg
    drawDisk x y i mid props =
        let color = colors !! i # fromMaybe "black" in
        H.rect $ [ P.x $ x - 25.0 + 2.5 * toNumber i
                 , P.y $ y - 5.0
                 , P.width $ show $ 50 - 5 * i
                 , P.height "10"
                 , H.attr "rx" "5"
                 , H.attr "ry" "5"
                 , H.class_ "hanoi-disk"
                 , P.fill color
                 ] <> props <> case mid of
                    Nothing → []
                    Just id → dndItemProps state
                        {   currentDragged: dragged
                        ,   draggable: true
                        ,   droppable: false
                        ,   id
                        }

    cursor ∷ {x ∷ Number, y ∷ Number} → Int → Html Msg
    cursor {x, y} i = drawDisk (x * 200.0) (y * 100.0) i Nothing [H.style "pointer-events" "none"]

    board ∷ Html Msg
    board =
        H.div (dndBoardProps <> [H.class_ "ui-board hanoi-board"])
        [   H.svg [H.class_ "hanoi-svg", P.viewBox 0 0 200 100] $
            [   H.g [] $ [0, 1, 2] <#> \i →
                    H.rect ([P.x $ toNumber $ 13 + 60 * i, P.y 10.0, P.width "54", P.height "90", P.fill "transparent"]
                        <> dndItemProps state
                        {   currentDragged: dragged
                        ,   draggable: false
                        ,   droppable: true
                        ,   id: i
                        }
                    )
            ,   H.g [] $ concat (
                    position # mapWithIndex \i stack →
                        stack # mapWithIndex \j k →
                            drawDisk
                                (toNumber $ 40 + 60 * i)
                                (toNumber $ 90 - 10 * j)
                                k
                                (if j == length stack - 1 then Just i else Nothing)
                                []
                )
            ,   H.fromMaybe $ cursor <$> pointer <*> (dragged >>= (position !! _) >>= last)
            ]
        ]

    rules =
        [   H.text "Le but du jeu est de déplacer tous les disques sur la tour de droite avec les contraintes suivantes:"
        ,   H.br
        ,   H.text "- tu peux déplacer seulement un disque à la fois;"
        ,   H.br
        ,   H.text "- tu ne peux pas déplacer un disque sur un disque plus petit que lui."
        ]

    winTitle = "Tu as gagné"