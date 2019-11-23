module Game.Roue.View where

import MyPrelude
import Lib.Util (map2)
import Game.Effs (EFFS)
import Game.Core (PointerPosition, _position, _pointer, _locked)
import Game.Roue.Model (State, Location(..), _size, _rotation, _dragged, setSizeA, rotateA, checkA, deleteDraggedA,
                        aligned, validRotation, validRotation')
import Pha (VDom, text, ifN, maybeN)
import Pha.Html (div', button, span, key, class', pc, click, pointerup, style, disabled)
import Pha.Svg (svg, path, viewBox, fill, stroke)
import UI.Template (template, card, dndBoardProps, dndItemProps)
import UI.Icons (icongroup, iconSelectGroup, ireset, irules)

colors :: Array String
colors = ["blue", "red", "magenta", "orange", "brown", "cyan", "gray", "black"]

polarToCartesian :: Number -> Number -> Number -> Number -> {x :: Number, y :: Number}
polarToCartesian centerX centerY radius angle = {
    x: centerX + radius * cos angle,
    y: centerY + radius * sin angle
}

pizza :: Number -> Number -> Number -> Number -> Number -> String
pizza cx cy radius startAngle endAngle =
    joinWith " " [
        "M", show cx, show cy,
        "L", show e.x, show e.y,
        "A", show radius, show radius, "0 0 0", show s.x, show s.y,
        "L", show cx, show cy
    ] where
        s = polarToCartesian cx cy radius startAngle
        e = polarToCartesian cx cy radius endAngle

innerWheel :: ∀a. Int -> VDom a EFFS
innerWheel size = div' [class' "roue-inner" true] [
    svg [viewBox 0 0 100 100] $ take size colors # mapWithIndex \i color ->
        path (pizza 50.0 50.0 50.0 (2.0 * pi * (toNumber i - 0.5) / toNumber size) (2.0 * pi * (toNumber i + 0.5) / toNumber size)) [
            fill color,
            stroke "black"
        ]
]

cursor :: ∀a. PointerPosition -> String -> VDom a EFFS
cursor {x, y} color = div' [
    class' "ui-cursor roue-select-color roue-cursor" true,
    style "left" $ pc x,
    style "top" $ pc y,
    style "background-color" color
] []


view :: State -> VDom State EFFS
view state = template _{config=config, board=board, rules=rules, winTitle=winTitle} state where
    size = state^._size
    position = state^._position
    valid = validRotation state

    config = card "Roue des couleurs" [
        iconSelectGroup state "Nombre de couleurs" [4, 5, 6, 7, 8] size setSizeA (const identity),        
        icongroup "Options" $ [ireset state, irules state]
    ]

    rules = [text "blah blah"]

    draggedColor :: Maybe String 
    draggedColor = state^._dragged >>= \d ->
        let colorIndex = case d of
                            Panel i -> i
                            Wheel i -> fromMaybe (-1) $ join (position !! i)
                            _ -> -1
        in colors !! colorIndex

    outerWheel = div' [
        class' "roue-outer" true,
        style "transform" $ "rotate(" <> show (360.0 * toNumber (state^._rotation) / toNumber size) <> "deg)"
    ] $
        [svg [key "svg", viewBox 0 0 100 100] $ map2 position (aligned state) \i pos align ->
            path (pizza 50.0 50.0 50.0 (2.0 * pi * (toNumber i - 0.5) / toNumber size) (2.0 * pi * (toNumber i + 0.5) / toNumber size)) ([
                class' "roue-wheel-part" true,
                fill $ if not align then  "#F0B27A" else if validRotation' state then "lightgreen" else "#F5B7B1"
            ] <> dndItemProps _dragged (isJust pos) true (Wheel i) state)
        ] <> (catMaybes $ position # mapWithIndex \index c -> c <#> \color -> 
            div' [
                class' "roue-outer-piece" true,
                key $ show index,
                style "left" $ pc $ 0.44 + 0.4 * cos(toNumber index * 2.0 * pi / toNumber size),
                style "top" $ pc $ 0.44 + 0.4 * sin(toNumber index * 2.0 * pi / toNumber size),
                style "background-color" $ colors !! color # fromMaybe ""
            ] []
        )

    board = div' (dndBoardProps _dragged <> [
        class' "roue-board" true,
        pointerup deleteDraggedA
    ]) [
        div' [class' "roue-buttons" true] $ concat [
            [button [
                class' "ui-button ui-button-primary roue-button" true,
                disabled $ state^._locked,
                click $ rotateA (-1)
            ] [text "↶"]],
            take size colors # mapWithIndex \i color ->
                div' ([
                    class' "roue-select-color ui-flex-center" true,
                    style "background-color" color
                ] <> dndItemProps _dragged true false (Panel i) state) [
                    ifN (elem (Just i) position) \_ ->
                        span [] [text "✓"]
                ],
            [button [
                class' "ui-button ui-button-primary roue-button" true,
                    disabled $ state^._locked,
                    click $ rotateA 1 -- lockAction n'est pas nécessaire
            ] [text "↷"]]
        ],
        div' [class' "roue-roue" true] [
            outerWheel,
            innerWheel size,
            button [
                class' "ui-button ui-button-primary roue-validate" true,
                disabled $ not valid || state^._locked,
                click checkA
            ] [text "Valider"],
            div' [class' "roue-valid-rotation" true] [
                if valid then
                    span [class' "valid" true] [text "✓"]
                else
                    span [class' "invalid" true] [text "✗"]
            ]
        ],
        maybeN $ cursor <$> state^._pointer <*> draggedColor
    ]

    winTitle = "GAGNÉ"