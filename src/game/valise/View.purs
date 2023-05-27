module Game.Valise.View where

import MamPrelude
import Game.Helpers (pointerDecoder)
import Game.Valise.Model (Model, Msg(..), _positions)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc, translate)
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import Web.PointerEvent.PointerEvent as PE

valise ∷ Model → Html Msg
valise model =
  S.svg
    [ SA.viewBox 0.0 0.0 825.0 690.0
    , E.onPointerMove' $ pointerDecoder MoveObject <<< PE.toMouseEvent
    , E.onPointerUp \_ → SetDrag Nothing
    ]
    [ S.use
        [ P.href "#valise"
        , H.class_ "valise-close"
        , SA.width "100%"
        , SA.height "100%"
        ]
    , S.g [ H.class_ "valise-open" ]
        [ S.use [ P.href "#openvalise" ]
        , object { symbol: "switch", link: Nothing, help: "", drag: false }
            300
            460
            42
            60
            [ E.onClick \_ → ToggleSwitch
            , H.style "transform" (if model.isSwitchOn then "scale(1,-1) translateY(-8%)" else "scale(1,1)")
            ]
            []

        , object { symbol: "bulboff", link: Nothing, help: "Trouve un moyen d'allumer l'ampoule", drag: false }
            477
            280
            48
            48
            []
            [ H.style "pointer-events" "all" ]

        , object { symbol: "bulbon", link: Just "noirblanc", help: "Jeu: tour noir, tout blanc", drag: false }
            477
            280
            48
            48
            [ H.style "transition" "opacity 0.5s"
            , SA.opacity $ if model.isSwitchOn then 1.0 else 0.0
            , H.style "pointer-events" $ if model.isSwitchOn then "all" else "none"
            ]
            []
        , object { symbol: "frog2", link: Just "frog", help: "Jeu: la grenouille", drag: false }
            549
            320
            40
            40
            [ SA.fill "#bcd35f"
            ]
            [ SA.x 10.0, SA.y 20.0, SA.width (pc 0.8), SA.height (pc 0.8) ]
        , object { symbol: "hanoibot", link: Just "hanoi", help: "Jeu: tours de Hanoi", drag: false }
            500
            430
            75
            51
            []
            [ SA.x (pc 0.3), SA.y (pc 0.2), SA.width (pc 0.4), SA.height (pc 0.4) ]
        , object { symbol: "knight", link: Just "queens", help: "Jeu: les 8 reines", drag: false }
            461
            380
            24
            48
            [ SA.transform "rotate(40)" ]
            []
        , object { symbol: "soli-peg2", link: Just "solitaire", help: "Jeu: solitaire", drag: false }
            492
            350
            40
            40
            []
            []
        , object { symbol: "pen", link: Just "dessin", help: "Jeu: dessin", drag: false }
            610
            400
            60
            60
            []
            []
        , object { symbol: "stack", link: Just "jetons", help: "Jeu: acquisition", drag: false }
            350
            500
            50
            50
            []
            []
        , object { symbol: "wheel", link: Just "roue", help: "Jeu: roue des couleurs", drag: false }
            400
            205
            50
            50
            [ SA.transform "scale(1,0.8)" ]
            []
        , object { symbol: "card", link: Just "bicolor", help: "Jeu: ???", drag: false }
            450
            130
            40
            50
            [ SA.transform "rotate(30)" ]
            []
        , object { symbol: "block2", link: Just "nim", help: "Jeu: bloque moi si tu peux", drag: false }
            380
            120
            40
            40
            []
            []
        , object { symbol: "tile", link: Just "tiling", help: "Jeu: carrelage", drag: false }
            280
            400
            120
            60
            []
            []
        , object { symbol: "tricolor2", link: Just "baseball", help: "Jeu: baseball multicolore", drag: false }
            350
            330
            90
            60
            []
            []
        , object { symbol: "traffic", link: Just "tricolor", help: "Jeu: feu tricolore", drag: false }
            250
            280
            64
            64
            []
            []
        , object { symbol: "race", link: Just "paths", help: "Jeu: chemins", drag: false }
            450
            445
            64
            64
            [ SA.transform "rotate(40)" ]
            []
        , object { symbol: "paw", link: Just "labete", help: "Jeu: la bête", drag: false }
            300
            180
            40
            40
            [ SA.transform "rotate(30)", SA.opacity 0.5 ]
            []
        , object { symbol: "quiet", link: Just "sansmot", help: "Jeu: preuve sans mot", drag: false }
            180
            130
            50
            50
            []
            []
        , object { symbol: "eternal-attack", link: Just "eternal", help: "Jeu: domination éternelle", drag: false }
            260
            125
            40
            40
            []
            []
        , object { symbol: "chocolate", link: Just "chocolat", help: "Jeu: chocolat", drag: false }
            200
            200
            60
            60
            [ SA.transform "rotate(40)" ]
            []

        , object { symbol: "flowerpot", link: Nothing, help: "Quelque chose se cache derrière ce pot", drag: true }
            533
            300
            64
            64
            []
            []
        , object { symbol: "hanoitop", link: Nothing, help: "Quelque chose se cache sous cette tour", drag: true }
            507
            409
            60
            57
            []
            []
        ]
    ]
  where
  object { drag, link, help, symbol } x' y' w' h' props children =
    let
      defaultTranslate = translate (pc $ toNumber x' / 850.0) (pc $ toNumber y' / 690.0)
    in
      S.g
        [ H.style "transform" $
            case drag, model ^. _positions ^. at symbol of
              true, Just { x: x2, y: y2 } → translate (pc x2) (pc y2)
              _, _ → defaultTranslate
        ]
        [ S.g props
            [ S.svg
                ( [ H.class_ "valise-object"
                  , H.class' "draggable" drag
                  , SA.width w'
                  , SA.height h'
                  , E.onPointerDown \_ →
                      if drag then
                        SetDrag (Just { name: symbol, x: toNumber w' / 1650.0, y: toNumber h' / 1380.0 })
                      else
                        NoAction
                  ]
                    <>
                      if isJust link then []
                      else
                        [ E.onPointerEnter \_ → ShowHelp help
                        , E.onPointerLeave \_ → ShowHelp ""
                        ]
                )
                [ S.use
                    [ P.href $ "#" <> symbol
                    , H.class_ "valise-symbol"
                    ]
                , H.maybe link \l →
                    H.a
                      [ P.href $ if l == "" then "" else "#" <> l ]
                      [ S.rect
                          ( [ SA.width "100%"
                            , SA.height "100%"
                            , H.class_ "valise-object-link"
                            , E.onPointerEnter \_ → ShowHelp help
                            , E.onPointerLeave \_ → ShowHelp ""
                            ]
                              <> children
                          )
                      ]
                ]
            ]
        ]

view ∷ Model → Html Msg
view model =
  H.div
    [ H.class_ "ui-flex-center valise-main-container"
    , H.class' "open" model.isOpen
    ]
    [ H.div []
        [ H.div [ H.class_ "valise-logo" ]
            [ S.svg [ SA.width "100%", SA.height "100%" ]
                [ S.use [ P.href "#logo" ] ]
            ]
        , H.div [ H.class_ "valise-container" ]
            [ valise model
            , H.div
                [ H.class_ "valise-help"
                , H.class' "visible" (model.helpVisible && model.help ≠ "")
                ]
                [ H.text model.help ]
            ]
        ]
    ]