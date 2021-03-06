module Game.Valise.View where
import MyPrelude
import Game.Valise.Model (State, Msg(..), _positions)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc, translate)
import Game.Common (pointerDecoder)

valise ∷ State → Html Msg
valise state =
    H.svg
    [   P.viewBox 0 0 825 690
    ,   E.on "pointermove" $ pointerDecoder >>> map (map MoveObject)
    ,   E.onPointerUp $ SetDrag Nothing
    ]
    [   H.use 
        [   P.href "#valise"
        ,   H.class_ "valise-close"
        ,   P.width "100%"
        ,   P.height "100%"
        ]
    ,   H.g [H.class_ "valise-open"]
        [   H.use [P.href "#openvalise"]
        ,   object { symbol: "switch", link: Nothing, help: "", drag: false } 
                    300 460 42 60
                    [   E.onClick ToggleSwitch
                    ,   H.style "transform" (if state.isSwitchOn then "scale(1,-1) translateY(-8%)" else "scale(1,1)")
                    ]
                    []

        ,   object { symbol: "bulboff", link: Nothing, help: "Trouve un moyen d'allumer l'ampoule", drag: false}
                    477 280 48 48
                    []
                    [   H.style "pointer-events" "all"]

        ,   object { symbol: "bulbon", link: Just "noirblanc", help: "Jeu: tour noir, tout blanc", drag: false } 
                    477 280 48 48
                    [   H.attr "transition" "opacity 0.5s"
                    ,   P.opacity $ if state.isSwitchOn then 1.0 else 0.0
                    ,   H.style "pointer-events" $ if state.isSwitchOn then "all" else "none"
                    ]
                    []
        ,   object { symbol: "frog2", link: Just "frog", help: "Jeu: la grenouille", drag: false}
                    549 320 40 40
                    [   P.fill "#bcd35f"
                    ]
                    [P.x 10.0, P.y 20.0, P.width "80%", P.height "80%"]
        ,   object { symbol: "hanoibot", link: Just "solitaire", help: "Jeu: solitaire", drag: false}
                    500 430 75 51
                    []
                    [H.attr "x" "30%", H.attr "y" "20%", P.width "40%", P.height "40%"]
        ,   object {symbol: "knight", link: Just "queens", help: "Jeu: les 8 reines", drag: false}
                    461 380 24 48
                    [P.transform "rotate(40)"]
                    []
        ,   object { symbol: "pen", link: Just "dessin", help: "Jeu: dessin", drag: false}
                    610 400 60 60
                    []
                    []
        ,   object {symbol: "stack", link: Just "jetons", help: "Jeu: acquisition", drag: false}
                    350 500 50 50
                    []
                    []
        ,   object {symbol: "wheel", link: Just "roue", help: "Jeu: roue des couleurs", drag: false}
                    400 205 50 50
                    [P.transform "scale(1,0.8)"]
                    []
        ,   object {symbol: "card", link: Just "bicolor", help: "Jeu: ???", drag: false}
                    450 130 40 50
                    [P.transform "rotate(30)"]
                    []
        ,   object {symbol: "block2", link: Just "nim", help: "Jeu: bloque moi si tu peux", drag: false}
                    380 120 40 40
                    []
                    []
        ,   object {symbol: "tile", link: Just "tiling", help: "Jeu: carrelage", drag: false}
                    280 400 120 60
                    []
                    []
        ,   object {symbol: "tricolor2", link: Just "baseball", help: "Jeu: baseball multicolore", drag: false}
                    350 330 90 60
                    []
                    []
        ,   object {symbol: "traffic", link: Just "tricolor", help: "Jeu: feu tricolore", drag: false}
                    250 280 64 64
                    []
                    []
        ,   object {symbol: "race", link: Just "paths", help: "Jeu: chemins", drag: false}
                    450 445 64 64
                    [P.transform "rotate(40)"]
                    []
        ,   object {symbol: "paw", link: Just "labete", help: "Jeu: la bête", drag: false}
                    300 180 40 40
                    [P.transform "rotate(30)", P.opacity 0.5]
                    []
        ,   object {symbol: "quiet", link: Just "sansmot", help: "Jeu: preuve sans mot", drag: false}
                    180 130 50 50
                    []
                    []
        ,   object {symbol: "eternal-attack", link: Just "eternal", help: "Jeu: domination éternelle", drag: false}
                    260 125 40 40
                    []
                    []
        ,   object {symbol: "chocolate", link: Just "chocolat", help: "Jeu: chocolat", drag: false}
                    200 200 60 60
                    [P.transform "rotate(40)"]
                    []

        ,   object { symbol: "flowerpot", link: Nothing, help: "Quelque chose se cache derrière ce pot", drag: true}
                    533 300 64 64
                    []
                    []
        ,   object { symbol: "hanoitop", link: Nothing, help: "Quelque chose se cache sous cette tour", drag: true}
                    507 409 60 57
                    []
                    []
        ]
    ] where
    object {drag, link, help, symbol} x' y' w' h' props children =
        let defaultTranslate = translate (pc $ toNumber x' / 850.0)  (pc $ toNumber y' / 690.0) in
        H.g
        [   H.style "transform" $ 
                case drag ∧ (state ^. _positions ^. at symbol) of
                    true ∧ Just {x: x2, y: y2} → translate (pc x2) (pc y2)    
                    _ → defaultTranslate
        ]
        [   H.g props
            [   H.svg (
                [   H.class_ "valise-object"
                ,   H.class' "draggable" drag
                ,   P.width $ show w'
                ,   P.height $ show h'
                ,   E.onPointerDown' $ if drag then 
                                        Just $ SetDrag (Just {name: symbol, x: toNumber w' / 1650.0, y: toNumber h' / 1380.0})
                                    else
                                        Nothing
                ]
                <> if isJust link then [] else
                [   E.onPointerEnter $ ShowHelp help
                ,   E.onPointerLeave $ ShowHelp ""
                ])
                [   H.use
                    [   P.href $ "#" <> symbol
                    ,   H.class_ "valise-symbol"
                    ]
                ,   H.maybe link \l →
                        H.a
                        [   P.href $ if l == "" then "" else "#" <> l]
                        [   H.rect (
                            [   P.width "100%"
                            ,   P.height "100%"
                            ,   H.class_ "valise-object-link"
                            ,   E.onPointerEnter $ ShowHelp help
                            ,   E.onPointerLeave $ ShowHelp ""
                            ] 
                            <> children
                            )
                        ]
                ]
            ]
        ]

view ∷ State → Html Msg 
view state =
    H.div
    [   H.class_ "ui-flex-center valise-main-container"
    ,   H.class' "open" state.isOpen
    ]
    [   H.div []
        [   H.div [H.class_ "valise-logo"]
            [   H.svg [P.width "100%", P.height "100%"]
                [   H.use [P.href "#logo"]]
            ]
        ,   H.div [H.class_ "valise-container"]
            [   valise state
            ,   H.div 
                [   H.class_ "valise-help"
                ,   H.class' "visible" (state.helpVisible && state.help ≠ "")
                ]
                [H.text state.help] 
            ]
        ]
    ]