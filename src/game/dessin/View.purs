module Game.Dessin.View (view) where
import MyPrelude
import Pha as H
import Pha.Elements as HH
import Pha.Attributes as P
import Pha.Events as E
import Game.Core (canPlay, isLevelFinished, _position, _pointer)
import Game.Dessin.Model (State, Msg(..), Graph, Position, Edge, (↔), edgesOf, nbRaises, _graph, _graphIndex)
import UI.Template (template, card, trackPointer)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)

currentLine ∷ ∀a. Position → Position → H.VDom a
currentLine p1 p2 =
    HH.line
    [   P.x1 $ show (100.0 * p1.x)
    ,   P.y1 $ show (100.0 * p1.y)
    ,   P.x2 $ show (20.0 * p2.x)
    ,   P.y2 $ show (20.0 * p2.y)
    ,   H.class_ "dessin-line-to-pointer"
    ]

getCoords ∷ Graph → Int → Maybe Position
getCoords graph u = graph.vertices !! u

getCoordsOfEdge ∷ Graph → Edge → Maybe {px1 ∷ Number, px2 ∷ Number, py1 ∷ Number, py2 ∷ Number}
getCoordsOfEdge graph (u ↔ v) = do
    {x: px1, y: py1} ← getCoords graph u
    {x: px2, y: py2} ← getCoords graph v
    pure {px1, px2, py1, py2}

view ∷ State → H.VDom Msg
view state = template {config, board, rules, winTitle} state where
    position = state^._position
    graph = state^._graph
    raises = nbRaises state
    s = if raises > 1 then "s" else ""
    levelFinished = isLevelFinished state

    config =
        card "Dessin" 
        [   iconSelectGroup state "Dessin" [0, 1, 2, 3, 4] (state^._graphIndex) SetGraphIndex
                \i → _{icon = IconText (show (i + 1)) }
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> (_ $ state)
        ]

    board =
        HH.div (trackPointer <> 
            [   H.class_ "ui-board dessin-board"
            ,   E.oncontextmenu $ Play Nothing
            ])
            [   HH.svg [H.class_ "dessin-svg", P.viewBox 0 0 100 100] $ concat 
                [   graph.edges <#> \edge →
                    H.maybe (getCoordsOfEdge graph edge) \{px1, px2, py1, py2} →
                        HH.line 
                        [   P.x1 $ show (20.0 * px1)
                        ,   P.y1 $ show (20.0 * py1)
                        ,   P.x2 $ show (20.0 * px2)
                        ,   P.y2 $ show (20.0 * py2)
                        ,   H.class_ "dessin-line1"
                        ]
                ,   edgesOf (state^._position) <#> \edge →
                    H.maybe (getCoordsOfEdge graph edge) \{px1, px2, py1, py2} → 
                        HH.line
                        [   P.x1 $ show (20.0 * px1)
                        ,   P.y1 $ show (20.0 * py1)
                        ,   P.x2 $ show (20.0 * px2)
                        ,   P.y2 $ show (20.0 * py2)
                        ,   H.class_ "dessin-line2"
                        ]
                ,   graph.vertices # mapWithIndex \i {x, y} →
                        HH.circle
                        [   P.cx $ show (20.0 * x)
                        ,   P.cy $ show (20.0 * y)
                        ,   P.r "3"
                        ,   P.stroke $ if Just (Just i) == last position then "red" else "blue"
                        ,   P.fill "blue"
                        ,   E.onclick $ Play (Just i)
                        ]
                ,   [H.when (not levelFinished) \_ →
                        H.maybeN $ currentLine <$> (state^._pointer) <*> (getCoords graph =<< join (last position))
                    ]
                ]
            ,   HH.span [H.class_ "dessin-raise-info dessin-raise-info"]
                [   H.text $ show raises <> " levé" <> s <> " de crayon"
                ]
            ,   HH.button
                [   H.class_ "ui-button ui-button-primary dessin-raise"
                ,   P.disabled $ not (canPlay state Nothing) || levelFinished
                ,   E.onclick $ Play Nothing
                ]
                [   H.text "Lever le crayon"]
            ]

    rules = 
        [   H.text "Le but du jeu est de dessiner le motif indiqué en pointillé en levant le moins souvent possible le crayon."
        ,   HH.br
        ,   H.text "Pour lever le crayon, tu peux cliquer sur le bouton prévu pour ou utiliser le clic droit."
        ]

    winTitle = "Tu as réussi en " <> show raises <> " levé" <> s