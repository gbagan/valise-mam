module Game.Roue.View where

import MyPrelude

import Game.Core (PointerPosition, _position, _pointer, _locked)
import Game.Roue.Model (State, Msg(..), Location(..), _size, _rotation, _dragged, aligned, validRotation, validRotation')
import Lib.Util (map2)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Keyed as K
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc)
import UI.Icons (icongroup, iconSelectGroup, ireset, irules)
import UI.Template (template, card, dndBoardProps, dndItemProps)

colors ∷ Array String
colors = ["blue", "red", "magenta", "orange", "brown", "cyan", "gray", "black"]

polarToCartesian ∷ Number → Number → Number → Number → {x ∷ Number, y ∷ Number}
polarToCartesian centerX centerY radius angle = {
    x: centerX + radius * cos angle,
    y: centerY + radius * sin angle
}


pizza ∷ Number → Number → Number → Number → Number → String
pizza cx cy radius startAngle endAngle =
    joinWith " " [
        "M", show cx, show cy,
        "L", show e.x, show e.y,
        "A", show radius, show radius, "0 0 0", show s.x, show s.y,
        "L", show cx, show cy
    ] where
        s = polarToCartesian cx cy radius startAngle
        e = polarToCartesian cx cy radius endAngle

innerWheel ∷ ∀a. Int → Html a
innerWheel size = H.div [H.class_ "roue-inner"] 
    [   H.svg [P.viewBox 0 0 100 100] $ take size colors # mapWithIndex \i color →
            H.path 
            [   P.d $ pizza 50.0 50.0 50.0 (2.0 * pi * (toNumber i - 0.5) / toNumber size) (2.0 * pi * (toNumber i + 0.5) / toNumber size)
            ,   P.fill color
            ,   P.stroke "black"
            ]
    ]

cursor ∷ ∀a. PointerPosition → String → Html a
cursor {x, y} color = H.div 
    [   H.class_ "ui-cursor roue-select-color roue-cursor"
    ,   H.style "left" $ pc x
    ,   H.style "top" $ pc y
    ,   H.style "background-color" color
    ] []


view ∷ State → Html Msg
view state = template {config, board, rules} state where
    size = state ^. _size
    position = state ^. _position
    valid = validRotation state
    dragged = state ^. _dragged
    pointer = state ^. _pointer
    locked = state ^. _locked

    config =
        card "Roue des couleurs"
        [   iconSelectGroup state "Nombre de couleurs" [4, 5, 6, 7, 8] size SetSize (const identity)
        ,   icongroup "Options" $ [ireset state, irules state]
        ]

    draggedColor ∷ Maybe String 
    draggedColor = state ^. _dragged >>= \d →
        let colorIndex = case d of
                            Panel i → i
                            Wheel i → fromMaybe (-1) $ join (position !! i)
                            _ → -1
        in colors !! colorIndex

    outerWheel = K.div 
        [   H.class_ "roue-outer"
        ,   H.style "transform" $ "rotate(" <> show (360.0 * toNumber (state ^. _rotation) / toNumber size) <> "deg)"
        ] $
        [   "svg" /\ H.svg [P.viewBox 0 0 100 100] (map2 position (aligned state) \i pos align →
                H.path (
                    [   P.d $ pizza
                                50.0
                                50.0
                                50.0
                                (2.0 * pi * (toNumber i - 0.5) / toNumber size)
                                (2.0 * pi * (toNumber i + 0.5) / toNumber size)
                    ,   H.class_ "roue-wheel-part"
                    ,   P.fill $ if not align then  "#F0B27A" else if validRotation' state then "lightgreen" else "#F5B7B1"
                    ] <> dndItemProps state
                        {   currentDragged: dragged
                        ,   draggable: isJust pos
                        ,   droppable: true
                        ,   id: Wheel i
                        }
                    )
        )] <> (catMaybes $ position # mapWithIndex \index c → c <#> \color → 
            show index /\ H.div
            [   H.class_ "roue-outer-piece"
            ,   H.style "left" $ pc $ 0.44 + 0.4 * cos(toNumber index * 2.0 * pi / toNumber size)
            ,   H.style "top" $ pc $ 0.44 + 0.4 * sin(toNumber index * 2.0 * pi / toNumber size)
            ,   H.style "background-color" $ colors !! color # fromMaybe ""
            ] []
        )

    drawButtons =
        H.div [H.class_ "roue-buttons"] $ concat
        [   [H.button [
                H.class_ "ui-button ui-button-primary roue-button",
                P.disabled locked,
                E.onClick $ Rotate (-1)
            ] [H.text "↶"]]
        ,   take size colors # mapWithIndex \i color →
                H.div ([
                    H.class_ "roue-select-color ui-flex-center",
                    H.style "background-color" color
                ] <> dndItemProps state {
                    currentDragged: dragged,
                    draggable: true,
                    droppable: false,
                    id: Panel i
                }) [
                    H.when (elem (Just i) position) \_ →
                        H.span [] [H.text "✓"]
                ]
        ,   [H.button [
                H.class_ "ui-button ui-button-primary roue-button",
                    P.disabled locked,
                    E.onClick $ Rotate 1 -- lockAction n'est pas nécessaire
            ] [H.text "↷"]]
        ]


    board =
        H.div (dndBoardProps <> [H.class_ "roue-board"])
        [   drawButtons
        ,   H.div [H.class_ "roue-roue"]
            [   outerWheel
            ,   innerWheel size
            ,   H.button
                [   H.class_ "ui-button ui-button-primary roue-validate"
                ,   P.disabled $ not valid || locked
                ,   E.onClick Check
                ]
                [H.text "Valider"]
            ,   H.div [H.class_ "roue-valid-rotation"]
                [   if valid then
                        H.span [H.class_ "valid"] [H.text "✓"]
                    else
                        H.span [H.class_ "invalid"] [H.text "✗"]
                ]
            ]
        ,   H.fromMaybe $ cursor <$> pointer <*> draggedColor
        ]

    rules =
        [   H.text "Le but du jeu est de poser une bille sur chaque emplacement de la roue et effectuer un tour complet de la roue en respectant la condition suivante:"
        ,   H.br
        ,   H.text "à chaque moment durant la rotation de la roue, exactement une bille a sa couleur qui correspond avec la couleur de la roue."
        ,   H.br
        ,   H.text "Tu peux tester en faisant varier le nombre de couleurs de la roue mais également en faisant varier le nombre de couleurs que tu utilises."
        ]