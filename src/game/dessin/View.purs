module Game.Dessin.View where
import MyPrelude
import Pha (VDom, text, maybeN)
import Pha.Action ((🔍))
import Pha.Html (div', button, svg, line, circle, span, br, class', click, contextmenu,
                 disabled, viewBox, stroke, fill, strokeWidth, strokeDasharray)
import Game.Core (canPlay, playA, _position, _pointer)
import Game.Effs (EFFS, preventDefault)
import Game.Dessin.Model (State, Graph, Position, Edge, (↔), edgesOf, nbRaises, setGraphIndexA, _graph, _graphIndex)
import UI.Template (template, card, trackPointer)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)

currentLine :: ∀a e. Position -> Position -> VDom a e
currentLine p1 p2 = line (100.0 * p1.x) (100.0 * p1.y) (20.0 * p2.x) (20.0 * p2.y) [class' "dessin-line-to-pointer" true]

getCoords :: Graph -> Int -> Position
getCoords graph u = graph.vertices !! u # fromMaybe {x: 0.0, y: 0.0}

getCoordsOfEdge :: Graph -> Edge -> {x1 :: Number, x2 :: Number, y1 :: Number, y2 :: Number}
getCoordsOfEdge graph (u ↔ v) = {x1, x2, y1, y2} where
    {x: x1, y: y1} = getCoords graph u
    {x: x2, y: y2} = getCoords graph v

view :: ∀a. Lens' a State -> State -> VDom a EFFS
view lens state = template lens (_{config=config, board=board, rules=rules, winTitle=winTitle}) state where
    position = state^._position
    graph = state^._graph
    raises = nbRaises state
    s = if raises > 1 then "s" else ""

    config = card "Dessin" [
        iconSelectGroup lens state "Dessin" [0, 1] (state^._graphIndex) setGraphIndexA (\i opt -> opt{icon = IconText (show (i + 1)) }),
        icongroup "Options" $ [iundo, iredo, ireset, irules] <#> \x -> x lens state
    ]

    board = div' (trackPointer lens <> [
                class' "ui-board dessin-board" true,
                contextmenu $ preventDefault *> (lens 🔍 playA Nothing)]) [
        svg [class' "dessin-svg" true, viewBox 0 0 100 100] $ concat [
            graph.edges <#> \edge ->
                let {x1, x2, y1, y2} = getCoordsOfEdge graph edge
                in line (20.0 * x1) (20.0 * y1) (20.0 * x2) (20.0 * y2) [stroke "grey", strokeDasharray "3,1"],
            edgesOf (state^._position) <#> \edge ->
                let {x1, x2, y1, y2} = getCoordsOfEdge graph edge
                in line (20.0 * x1) (20.0 * y1) (20.0 * x2) (20.0 * y2) [stroke "red", strokeWidth "1.5"],
            graph.vertices # mapWithIndex \i  {x, y} ->
                circle (20.0 * x) (20.0 * y) 3.0 [
                    stroke $ if Just (Just i) == last position then "red" else "blue",
                    fill "blue",
                    click $ lens 🔍 playA (Just i)
                ],
            [maybeN $ currentLine <$> (state^._pointer) <*> (getCoords graph <$> join (last position))]
        ],
        span [class' "dessin-raise-info dessin-raise-info" true] [
            text $ show raises <> " levé" <> s <> " de crayon"
        ],
        button [
            class' "ui-button ui-button-primary dessin-raise" true,
            disabled $ not (canPlay state Nothing),
            click $ lens 🔍 playA Nothing
        ] [text "Lever le crayon"]
    ]

    rules = [
        text "blah blah blah blah blah blah blah blah", br,
        text "blah blah blah blah blah blah blah blah"
    ]


    winTitle = "Tu as réussi en " <> show raises <> " levé" <> s