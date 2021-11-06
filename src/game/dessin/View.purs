module Game.Dessin.View (view) where
import MyPrelude
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Game.Core (canPlay, isLevelFinished, _position, _pointer)
import Lib.Graph (Position)
import Lib.Graph as Graph
import Game.Dessin.Model (State, Msg(..), Move(..), GraphIndex(..),
                         graphs, nbGraphs, edgesOf, nbRaises, _graph, _graphIndex, _graphEditor)
import UI.Template (template, card, trackPointer, bestScoreDialog)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup, iconBestScore, iundo, iredo, ireset, irules)
import UI.GraphEditor as GEditor

currentLine ∷ ∀a. Position → Position → Html a
currentLine p1 p2 =
    H.line
    [   P.x1 $ 100.0 * p1.x
    ,   P.y1 $ 100.0 * p1.y
    ,   P.x2 $ 100.0 * p2.x
    ,   P.y2 $ 100.0 * p2.y
    ,   H.class_ "dessin-line-to-pointer"
    ]

view ∷ State → Html Msg
view state = template {config, board, rules, winTitle, scoreDialog, customDialog} state where
    position = state^._position
    graph = state^._graph
    graphIndex = state^._graphIndex
    raises = nbRaises state
    s = if raises > 1 then "s" else ""
    levelFinished = isLevelFinished state

    config =
        card "Dessin"
        [   iconSelectGroup state "Niveau" ((GraphIndex <$> 0..(nbGraphs-1)) <> [CustomGraph]) graphIndex SetGraphIndex
                case _ of 
                    GraphIndex i → _{icon = IconText (show (i + 1)), tooltip = graphs !! i <#> _.title}
                    _ → _{icon = IconSymbol "#customize", tooltip = Just "Crée ta propre pièce"}
        ,   icongroup "Options" [iundo state, iredo state, ireset state, irules state]
        ,   iconBestScore state
        ]

    board =
        H.div (trackPointer <>
            [   H.class_ "ui-board dessin-board"
            ,   E.onContextMenu \_ → Play Raise
            ])
            [   H.svg [H.class_ "dessin-svg", P.viewBox 0 0 100 100] $ concat 
                [   graph.edges <#> \edge →
                    H.maybe (Graph.getCoordsOfEdge graph edge) \{px1, px2, py1, py2} →
                        H.line 
                        [   P.x1 $ 100.0 * px1
                        ,   P.y1 $ 100.0 * py1
                        ,   P.x2 $ 100.0 * px2
                        ,   P.y2 $ 100.0 * py2
                        ,   H.class_ "dessin-line1"
                        ]
                ,   edgesOf (state^._position) <#> \edge →
                    H.maybe (Graph.getCoordsOfEdge graph edge) \{px1, px2, py1, py2} → 
                        H.line
                        [   P.x1 $ 100.0 * px1
                        ,   P.y1 $ 100.0 * py1
                        ,   P.x2 $ 100.0 * px2
                        ,   P.y2 $ 100.0 * py2
                        ,   H.class_ "dessin-line2"
                        ]
                ,   if not levelFinished then
                        graph.vertices # mapWithIndex \i {x, y} →
                            H.circle
                            [   P.cx $ 100.0 * x
                            ,   P.cy $ 100.0 * y
                            ,   P.r 3.0
                            ,   P.stroke $ if Just (MoveTo i) == last position then "red" else "blue"
                            ,   P.fill "blue"
                            ,   E.onClick \_ → Play (MoveTo i)
                            ]
                    else
                        []
                ,   [H.when (not levelFinished) \_ →
                        H.fromMaybe case last position of
                            Just (MoveTo x) → currentLine <$> (state^._pointer) <*> (Graph.getCoords graph x)
                            _ → Nothing
                    ]
                ]
            ,   H.span [H.class_ "dessin-title"]
                [   H.text $ graph.title
                ]
            ,   H.span [H.class_ "dessin-raise-info"]
                [   H.text $ show raises <> " levé" <> s <> " de crayon"
                ]
            ,   H.button
                [   H.class_ "ui-button ui-button-primary dessin-raise"
                ,   P.disabled $ not (canPlay state Raise) || levelFinished
                ,   E.onClick \_ → Play Raise
                ]
                [   H.text "Lever le crayon"]
            ]

    scoreDialog _ = bestScoreDialog state \bestPos → [
        H.div [H.class_ "ui-board dessin-bestscore"]
        [   H.svg [H.class_ "dessin-svg", P.viewBox 0 0 100 100] $ concat 
            [   graph.edges <#> \edge →
                H.maybe (Graph.getCoordsOfEdge graph edge) \{px1, px2, py1, py2} →
                    H.line 
                    [   P.x1 $ 100.0 * px1
                    ,   P.y1 $ 100.0 * py1
                    ,   P.x2 $ 100.0 * px2
                    ,   P.y2 $ 100.0 * py2
                    ,   H.class_ "dessin-line2"
                    ]
            ,   edgesOf bestPos # mapWithIndex \i edge →
                H.maybe (Graph.getCoordsOfEdge graph edge) \{px1, px2, py1, py2} → 
                    H.text_ (show (i + 1))
                    [   P.x $ 50.0 * (px1 + px2)
                    ,   P.y $ 50.0 * (py1 + py2) + 2.0
                    ,   H.class_ "dessin-edge-no"
                    ]
            ]
        ]
    ]

    customDialog _ = GEditor.view (state^._graphEditor) CloseEditor

    rules = 
        [   H.text "Le but du jeu est de dessiner le motif indiqué en pointillé en levant le moins souvent possible le crayon."
        ,   H.br
        ,   H.text "Pour lever le crayon, tu peux cliquer sur le bouton prévu pour ou utiliser le clic droit."
        ]

    winTitle = "Tu as réussi en " <> show raises <> " levé" <> s