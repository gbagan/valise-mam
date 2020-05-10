module Game.Dessin.View (view) where
import MyPrelude
import Pha (VDom, text, maybeN, (<??>),class_)
import Pha.Elements (div, button, span, br)
import Pha.Events (onclick, oncontextmenu)
import Pha.Attributes (disabled)
import Pha.Svg (svg, line, circle, viewBox, stroke, fill, x1_, x2_, y1_, y2_, cx, cy, r_)
import Game.Core (canPlay, _position, _pointer)
import Game.Dessin.Model (State, Msg(..), Graph, Position, Edge, (↔), edgesOf, nbRaises, _graph, _graphIndex)
import UI.Template (template, card, trackPointer)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)

currentLine ∷ ∀a. Position → Position → VDom a
currentLine p1 p2 =
    line
    [   x1_ $ show (100.0 * p1.x)
    ,   y1_ $ show (100.0 * p1.y)
    ,   x2_ $ show (20.0 * p2.x)
    ,   y2_ $ show (20.0 * p2.y)
    ,   class_ "dessin-line-to-pointer"
    ]

getCoords ∷ Graph → Int → Maybe Position
getCoords graph u = graph.vertices !! u

getCoordsOfEdge ∷ Graph → Edge → Maybe {px1 ∷ Number, px2 ∷ Number, py1 ∷ Number, py2 ∷ Number}
getCoordsOfEdge graph (u ↔ v) = do
    {x: px1, y: py1} ← getCoords graph u
    {x: px2, y: py2} ← getCoords graph v
    pure {px1, px2, py1, py2}

view ∷ State → VDom Msg
view state = template {config, board, rules, winTitle} state where
    position = state^._position
    graph = state^._graph
    raises = nbRaises state
    s = if raises > 1 then "s" else ""

    config =    
        card "Dessin" 
        [   iconSelectGroup state "Dessin" [0, 1, 2, 3, 4] (state^._graphIndex) SetGraphIndex
                \i → _{icon = IconText (show (i + 1)) }
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> (_ $ state)
        ]

    board =
        div (trackPointer <> 
            [   class_ "ui-board dessin-board"
            ,   oncontextmenu $ Play Nothing
            ])
            [   svg [class_ "dessin-svg", viewBox 0 0 100 100] $ concat 
                [   graph.edges <#> \edge →
                    getCoordsOfEdge graph edge <??> \{px1, px2, py1, py2} →
                        line 
                        [   x1_ $ show (20.0 * px1)
                        ,   y1_ $ show (20.0 * py1)
                        ,   x2_ $ show (20.0 * px2)
                        ,   y2_ $ show (20.0 * py2)
                        ,   class_ "dessin-line1"
                        ]
                ,   edgesOf (state^._position) <#> \edge →
                    getCoordsOfEdge graph edge <??> \{px1, px2, py1, py2} → 
                        line
                        [   x1_ $ show (20.0 * px1)
                        ,   y1_ $ show (20.0 * py1)
                        ,   x2_ $ show (20.0 * px2)
                        ,   y2_ $ show (20.0 * py2)
                        ,   class_ "dessin-line2"
                        ]
                ,   graph.vertices # mapWithIndex \i {x, y} →
                        circle
                        [   cx $ show (20.0 * x)
                        ,   cy $ show (20.0 * y)
                        ,   r_ "3"
                        ,   stroke $ if Just (Just i) == last position then "red" else "blue"
                        ,   fill "blue"
                        ,   onclick $ Play (Just i)
                        ]
                ,   [maybeN $ currentLine <$> (state^._pointer) <*> (getCoords graph =<< join (last position))]
                ]
            ,   span [class_ "dessin-raise-info dessin-raise-info"] [
                text $ show raises <> " levé" <> s <> " de crayon"
                ]
            ,   button
                [   class_ "ui-button ui-button-primary dessin-raise"
                ,   disabled $ not (canPlay state Nothing)
                ,   onclick $ Play Nothing
                ]
                [   text "Lever le crayon"]
            ]

    rules = 
        [   text "Le but du jeu est de dessiner le motif indiqué en pointillé en levant le moins souvent possible le crayon."
        ,   br
        ,   text "Pour lever le crayon, tu peux cliquer sur le bouton prévu pour ou utiliser le clic droit."
        ]

    winTitle = "Tu as réussi en " <> show raises <> " levé" <> s