module Game.Roue.View where

import MyPrelude
import Lib.Util (map2)
import Game.Core (PointerPosition, _position, _pointer, _locked)
import Game.Roue.Model (State, Msg(..), Location(..), _size, _rotation, _dragged,
                        aligned, validRotation, validRotation')
import Pha (VDom, text, (<&&>), maybeN, key, class_, class', style)
import Pha.Elements (div, button, span)
import Pha.Events (onclick)
import Pha.Attributes (disabled)
import Pha.Svg (svg, path, viewBox, fill, stroke)
import Pha.Util (pc)
import UI.Template (template, card, dndBoardProps, dndItemProps)
import UI.Icons (icongroup, iconSelectGroup, ireset, irules)

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

innerWheel ∷ ∀a. Int → VDom a
innerWheel size = div [class' "roue-inner" true] [
    svg [viewBox 0 0 100 100] $ take size colors # mapWithIndex \i color →
        path (pizza 50.0 50.0 50.0 (2.0 * pi * (toNumber i - 0.5) / toNumber size) (2.0 * pi * (toNumber i + 0.5) / toNumber size)) [
            fill color,
            stroke "black"
        ]
]

cursor ∷ ∀a. PointerPosition → String → VDom a
cursor {x, y} color = div [
    class' "ui-cursor roue-select-color roue-cursor" true,
    style "left" $ pc x,
    style "top" $ pc y,
    style "background-color" color
] []


view ∷ State → VDom Msg
view state = template {config, board, rules} state where
    size = state^._size
    position = state^._position
    valid = validRotation state

    config =
        card "Roue des couleurs"
        [   iconSelectGroup state "Nombre de couleurs" [4, 5, 6, 7, 8] size SetSize (const identity)
        ,   icongroup "Options" $ [ireset state, irules state]
        ]

    draggedColor ∷ Maybe String 
    draggedColor = state^._dragged >>= \d →
        let colorIndex = case d of
                            Panel i → i
                            Wheel i → fromMaybe (-1) $ join (position !! i)
                            _ → -1
        in colors !! colorIndex

    outerWheel = div [
        class' "roue-outer" true,
        style "transform" $ "rotate(" <> show (360.0 * toNumber (state^._rotation) / toNumber size) <> "deg)"
    ] $
        [svg [key "svg", viewBox 0 0 100 100] $ map2 position (aligned state) \i pos align →
            path 
                (pizza
                    50.0
                    50.0
                    50.0
                    (2.0 * pi * (toNumber i - 0.5) / toNumber size)
                    (2.0 * pi * (toNumber i + 0.5) / toNumber size)
                ) (
                [   class_ "roue-wheel-part"
                ,   fill $ if not align then  "#F0B27A" else if validRotation' state then "lightgreen" else "#F5B7B1"
                ] <> dndItemProps 
                    {   currentDragged: state^._dragged
                    ,   draggable: isJust pos
                    ,   droppable: true
                    ,   id: Wheel i
                    } state
                )
        ] <> (catMaybes $ position # mapWithIndex \index c → c <#> \color → 
            div
            [   class_ "roue-outer-piece"
            ,   key $ show index
            ,   style "left" $ pc $ 0.44 + 0.4 * cos(toNumber index * 2.0 * pi / toNumber size)
            ,   style "top" $ pc $ 0.44 + 0.4 * sin(toNumber index * 2.0 * pi / toNumber size)
            ,   style "background-color" $ colors !! color # fromMaybe ""
            ] []
        )

    drawButtons =
        div [class_ "roue-buttons"] $ concat
        [   [button [
                class_ "ui-button ui-button-primary roue-button",
                disabled $ state^._locked,
                onclick $ Rotate (-1)
            ] [text "↶"]]
        ,   take size colors # mapWithIndex \i color →
                div ([
                    class_ "roue-select-color ui-flex-center",
                    style "background-color" color
                ] <> dndItemProps {
                    currentDragged: state^._dragged,
                    draggable: true,
                    droppable: false,
                    id: Panel i
                } state) [
                    elem (Just i) position <&&> \_ →
                        span [] [text "✓"]
                ]
        ,   [button [
                class_ "ui-button ui-button-primary roue-button",
                    disabled $ state^._locked,
                    onclick $ Rotate 1 -- lockAction n'est pas nécessaire
            ] [text "↷"]]
        ]


    board =
        div (dndBoardProps <> [class_ "roue-board"])
        [   drawButtons
        ,   div [class_ "roue-roue"]
            [   outerWheel
            ,   innerWheel size
            ,   button
                [   class_ "ui-button ui-button-primary roue-validate"
                ,   disabled $ not valid || state^._locked
                ,   onclick Check
                ]
                [text "Valider"]
            ,   div [class_ "roue-valid-rotation"]
                [   if valid then
                        span [class_ "valid"] [text "✓"]
                    else
                        span [class_ "invalid"] [text "✗"]
                ]
            ]
        ,   maybeN $ cursor <$> state^._pointer <*> draggedColor
        ]

    rules = [text "blah blah"]