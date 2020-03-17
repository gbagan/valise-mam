module Game.Valise.View where
import MyPrelude
import Game.Valise.Model (State, Msg(..), _positions)
import Pha (VDom, h, text, (<??>), class_, class', attr, style)
import Pha.Elements (div, a)
import Pha.Attributes (href)
import Pha.Events (onclick, on, onpointerenter, onpointerleave, onpointerup, onpointerdown')
import Pha.Util (pc, translate)
import Pha.Svg (svg, use, g, rect, width, height, x_, y_, viewBox, fill, transform)
import Game.Common (pointerDecoder)

valise ∷ State → VDom Msg
valise state =
    svg
    [   viewBox 0 0 825 690
    ,   on "pointermove" $ pointerDecoder >>> (map MoveObject)
    ,   onpointerup $ SetDrag Nothing
    ]
    [   h "use" [href "#valise", class_ "valise-close", width "100%", height "100%"] []
    ,   g [class_ "valise-open"]
        [   h "use" [href "#openvalise"] []
        ,   object { symbol: "switch", link: Nothing, help: "", drag: false } 
                    300 460 42 60
                    [   onclick ToggleSwitch
                    ,   style "transform" (if state.isSwitchOn then "scale(1,-1) translateY(-8%)" else "scale(1,1)")
                    ]
                    []

        ,   object { symbol: "bulboff", link: Nothing, help: "Trouve un moyen d'allumer l'ampoule", drag: false}
                    477 280 48 48
                    []
                    [   style "pointer-events" "all"]

        ,   object { symbol: "bulbon", link: Just "noirblanc", help: "Jeu: tour noir, tout blanc", drag: false } 
                    477 280 48 48
                    [   attr "transition" "opacity 0.5s"
                    ,   attr "opacity" $ if state.isSwitchOn then "1" else "0"
                    ,   style "pointer-events" $ if state.isSwitchOn then "all" else "none"
                    ]
                    []
        ,   object { symbol: "frog2", link: Just "frog", help: "Jeu: la grenouille", drag: false}
                    549 320 40 40
                    [   fill "#bcd35f"
                    ]
                    [x_ "10%", y_ "20%", width "80%", height "80%"]
        ,   object { symbol: "flowerpot", link: Nothing, help: "Quelque chose se cache derrière ce pot", drag: true}
                    533 300 64 64
                    []
                    []
        ,   object { symbol: "hanoibot", link: Just "solitaire", help: "Jeu: solitaire", drag: false}
                    500 430 75 51
                    []
                    [x_ "30%", y_ "20%", width "40%", height "40%"]
        ,   object { symbol: "hanoitop", link: Nothing, help: "Quelque chose se cache sous cette tour", drag: true}
                    507 409 60 57
                    []
                    []
        ,   object {symbol: "knight", link: Just "queens", help: "Jeu: les 8 reines", drag: false}
                    461 380 24 48
                    [transform "rotate(40)"]
                    []
        ,   object { symbol: "pen", link: Just "dessin", help: "Jeu: dessin", drag: false}
                    610 400 60 60
                    []
                    []
        ,   object {symbol: "stack", link: Just "jetons", help: "Jeu: jetons", drag: false}
                    350 500 50 50
                    []
                    []
        ,   object {symbol: "wheel", link: Just "roue", help: "Jeu: roue des couleurs", drag: false}
                    400 205 50 50
                    [transform "scale(1,0.8)"]
                    []
        ,   object {symbol: "card", link: Just "nim", help: "Jeu: Poker Nim", drag: false}
                    450 130 40 50
                    [transform "rotate(30)"]
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
                    [transform "rotate(40)"]
                    []
        ,   object {symbol: "paw", link: Just "labete", help: "Jeu: la bête", drag: false}
                    300 180 40 40
                    [transform "rotate(30)", attr "opacity" "0.5"]
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
                    [transform "rotate(40)"]
                    []
        ]
    ] where
    object {drag, link, help, symbol} x' y' w' h' props children =
        let defaultTranslate = translate (pc $ toNumber x' / 850.0)  (pc $ toNumber y' / 690.0) in
        g 
        [   style "transform" $ 
                case drag ∧ (state ^. _positions ^. at symbol) of
                    true ∧ Just {x: x2, y: y2} → translate (pc x2) (pc y2)    
                    _ → defaultTranslate
        ]
        [   g props
            [   svg (
                [   class_ "valise-object"
                ,   class' "draggable" drag
                ,   width $ show w'
                ,   height $ show h'
                ,   onpointerdown' $ if drag then 
                                        Just $ SetDrag (Just {name: symbol, x: toNumber w' / 1650.0, y: toNumber h' / 1380.0})
                                    else
                                        Nothing
                ]
                <> if isJust link then [] else
                [   onpointerenter $ ShowHelp help
                ,   onpointerleave $ ShowHelp ""
                ])
                [   h "use"
                    [   href $ "#" <> symbol, class_ "valise-symbol"
                    ] []
                ,   link <??> \l →
                        a
                        [href l]
                        [   rect (
                            [   width "100%"
                            ,   height "100%"
                            ,   class_ "valise-object-link"
                            ,   onpointerenter $ ShowHelp help
                            ,   onpointerleave $ ShowHelp ""
                            ] 
                            <> children
                            )
                        ]
                ]
            ]
        ]

view ∷ State → VDom Msg 
view state =
    div
    [   class_ "ui-flex-center valise-main-container"
    ,   class' "open" state.isOpen
    ]
    [   div []
        [   div [class_ "valise-logo"]
            [   svg [width "100%", height "100%"]
                [   use [href "#logo"]]
            ]
        ,   div [class_ "valise-container"]
            [   valise state
            ,   div 
                [   class_ "valise-help"
                ,   class' "visible" (state.helpVisible && state.help /= "")
                ]
                [text state.help] 
            ]
        ]
    ]