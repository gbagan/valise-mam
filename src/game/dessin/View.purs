module Game.Dessin.View (view) where
import MyPrelude
import Pha as H
import Pha.Elements as HH
import Pha.Attributes as P
import Pha.Events as E
import Game.Core (canPlay, isLevelFinished, _position, _pointer)
import Game.Dessin.Model (State, Msg(..), Graph, Position, Move(..), Edge, (↔),
                         graphs, nbGraphs, edgesOf, nbRaises, _graph, _graphIndex)
import UI.Template (template, card, trackPointer, bestScoreDialog)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup, iconBestScore, iundo, iredo, ireset, irules)

currentLine ∷ ∀a. Position → Position → H.VDom a
currentLine p1 p2 =
    HH.line
    [   P.x1 $ 100.0 * p1.x
    ,   P.y1 $ 100.0 * p1.y
    ,   P.x2 $ 20.0 * p2.x
    ,   P.y2 $ 20.0 * p2.y
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
view state = template {config, board, rules, winTitle, scoreDialog} state where
    position = state^._position
    graph = state^._graph
    raises = nbRaises state
    s = if raises > 1 then "s" else ""
    levelFinished = isLevelFinished state

    config =
        card "Dessin"
        [   iconSelectGroup state "Niveau" (0..(nbGraphs-1)) (state^._graphIndex) SetGraphIndex
                \i → _{icon = IconText (show (i + 1)), tooltip = graphs !! i <#> _.title}
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> (_ $ state)
        ,   iconBestScore state
        ]

    board =
        HH.div (trackPointer <>
            [   H.class_ "ui-board dessin-board"
            ,   E.oncontextmenu $ Play Raise
            ])
            [   HH.svg [H.class_ "dessin-svg", P.viewBox 0 0 100 100] $ concat 
                [   graph.edges <#> \edge →
                    H.maybe (getCoordsOfEdge graph edge) \{px1, px2, py1, py2} →
                        HH.line 
                        [   P.x1 $ 20.0 * px1
                        ,   P.y1 $ 20.0 * py1
                        ,   P.x2 $ 20.0 * px2
                        ,   P.y2 $ 20.0 * py2
                        ,   H.class_ "dessin-line1"
                        ]
                ,   edgesOf (state^._position) <#> \edge →
                    H.maybe (getCoordsOfEdge graph edge) \{px1, px2, py1, py2} → 
                        HH.line
                        [   P.x1 $ 20.0 * px1
                        ,   P.y1 $ 20.0 * py1
                        ,   P.x2 $ 20.0 * px2
                        ,   P.y2 $ 20.0 * py2
                        ,   H.class_ "dessin-line2"
                        ]
                ,   if not levelFinished then
                        graph.vertices # mapWithIndex \i {x, y} →
                            HH.circle
                            [   P.cx $ 20.0 * x
                            ,   P.cy $ 20.0 * y
                            ,   P.r 3.0
                            ,   P.stroke $ if Just (MoveTo i) == last position then "red" else "blue"
                            ,   P.fill "blue"
                            ,   E.onclick $ Play (MoveTo i)
                            ]
                    else
                        []
                ,   [H.when (not levelFinished) \_ →
                        H.maybeN case last position of
                            Just (MoveTo x) → currentLine <$> (state^._pointer) <*> (getCoords graph x)
                            _ → Nothing
                    ]
                ]
            ,   HH.span [H.class_ "dessin-title"]
                [   H.text $ graph.title
                ]
            ,   HH.span [H.class_ "dessin-raise-info"]
                [   H.text $ show raises <> " levé" <> s <> " de crayon"
                ]
            ,   HH.button
                [   H.class_ "ui-button ui-button-primary dessin-raise"
                ,   P.disabled $ not (canPlay state Raise) || levelFinished
                ,   E.onclick $ Play Raise
                ]
                [   H.text "Lever le crayon"]
            ]

    scoreDialog _ = bestScoreDialog state \bestPos → [
        HH.div [H.class_ "ui-board dessin-bestscore"]
        [   HH.svg [H.class_ "dessin-svg", P.viewBox 5 5 90 90] $ concat 
            [   graph.edges <#> \edge →
                H.maybe (getCoordsOfEdge graph edge) \{px1, px2, py1, py2} →
                    HH.line 
                    [   P.x1 $ 20.0 * px1
                    ,   P.y1 $ 20.0 * py1
                    ,   P.x2 $ 20.0 * px2
                    ,   P.y2 $ 20.0 * py2
                    ,   H.class_ "dessin-line2"
                    ]
            ,   edgesOf bestPos # mapWithIndex \i edge →
                H.maybe (getCoordsOfEdge graph edge) \{px1, px2, py1, py2} → 
                    HH.text (show (i + 1))
                    [   P.x $ 10.0 * (px1 + px2)
                    ,   P.y $ 10.0 * (py1 + py2) + 2.0
                    ,   H.class_ "dessin-edge-no"
                    ]
            ]
        ]
    ]

    rules = 
        [   H.text "Le but du jeu est de dessiner le motif indiqué en pointillé en levant le moins souvent possible le crayon."
        ,   HH.br
        ,   H.text "Pour lever le crayon, tu peux cliquer sur le bouton prévu pour ou utiliser le clic droit."
        ]

    winTitle = "Tu as réussi en " <> show raises <> " levé" <> s