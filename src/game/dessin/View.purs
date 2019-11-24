module Game.Dessin.View (view) where
import MyPrelude
import Pha (VDom, text, maybeN, class_, class')
import Pha.Elements (div, button, span, br)
import Pha.Attributes (onclick, oncontextmenu', disabled)
import Pha.Svg (svg, line, circle, viewBox, stroke, fill, x1, x2, y1, y2, cx, cy, r, strokeWidth, strokeDasharray)
import Pha.Event (preventDefault)
import Game.Core (canPlay, playA, _position, _pointer)
import Game.Effs (EFFS)
import Game.Dessin.Model (State, Graph, Position, Edge, (↔), edgesOf, nbRaises, setGraphIndexA, _graph, _graphIndex)
import UI.Template (template, card, trackPointer)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)

currentLine :: ∀a e. Position -> Position -> VDom a e
currentLine p1 p2 = line [
    x1 $ show (100.0 * p1.x),
    y1 $ show (100.0 * p1.y),
    x2 $ show (20.0 * p2.x),
    y2 $ show (20.0 * p2.y),
    class_ "dessin-line-to-pointer"
]

getCoords :: Graph -> Int -> Position
getCoords graph u = graph.vertices !! u # fromMaybe {x: 0.0, y: 0.0}

getCoordsOfEdge :: Graph -> Edge -> {px1 :: Number, px2 :: Number, py1 :: Number, py2 :: Number}
getCoordsOfEdge graph (u ↔ v) = {px1, px2, py1, py2} where
    {x: px1, y: py1} = getCoords graph u
    {x: px2, y: py2} = getCoords graph v

view :: State -> VDom State EFFS
view state = template _{config=config, board=board, rules=rules, winTitle=winTitle} state where
    position = state^._position
    graph = state^._graph
    raises = nbRaises state
    s = if raises > 1 then "s" else ""

    config = card "Dessin" [
        iconSelectGroup state "Dessin" [0, 1, 2, 3, 4] (state^._graphIndex) setGraphIndexA \i -> _{icon = IconText (show (i + 1)) },
        icongroup "Options" $ [iundo, iredo, ireset, irules] <#> \x -> x state
    ]

    board = div (trackPointer <> [
                class_ "ui-board dessin-board",
                -- todo
                oncontextmenu' \ev -> preventDefault ev *> playA Nothing]) [
        svg [class' "dessin-svg" true, viewBox 0 0 100 100] $ concat [
            graph.edges <#> \edge ->
                let {px1, px2, py1, py2} = getCoordsOfEdge graph edge
                in line [
                    x1 $ show (20.0 * px1),
                    y1 $ show (20.0 * py1),
                    x2 $ show (20.0 * px2),
                    y2 $ show (20.0 * py2),
                    stroke "grey",
                    strokeDasharray "3,1"
                ],
            edgesOf (state^._position) <#> \edge ->
                let {px1, px2, py1, py2} = getCoordsOfEdge graph edge
                in line [
                    x1 $ show (20.0 * px1),
                    y1 $ show (20.0 * py1),
                    x2 $ show (20.0 * px2),
                    y2 $ show (20.0 * py2),
                    stroke "red",
                    strokeWidth "1.5"
                ],
            graph.vertices # mapWithIndex \i  {x, y} ->
                circle [
                    cx $ show (20.0 * x),
                    cy $ show (20.0 * y),
                    r "3",
                    stroke $ if Just (Just i) == last position then "red" else "blue",
                    fill "blue",
                    onclick $ playA (Just i)
                ],
            [maybeN $ currentLine <$> (state^._pointer) <*> (getCoords graph <$> join (last position))]
        ],
        span [class' "dessin-raise-info dessin-raise-info" true] [
            text $ show raises <> " levé" <> s <> " de crayon"
        ],
        button [
            class' "ui-button ui-button-primary dessin-raise" true,
            disabled $ not (canPlay state Nothing),
            onclick $ playA Nothing
        ] [text "Lever le crayon"]
    ]

    rules = [
        text "blah blah blah blah blah blah blah blah", br,
        text "blah blah blah blah blah blah blah blah"
    ]

    winTitle = "Tu as réussi en " <> show raises <> " levé" <> s