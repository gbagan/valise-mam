module Game.Dessin.View (view) where
import MyPrelude
import Pha (VDom, text, maybeN)
import Pha.Html (div', button, span, br, class', click, contextmenu', disabled)
import Pha.Svg (svg, line, circle, viewBox, stroke, fill, strokeWidth, strokeDasharray)
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

    board = div' (trackPointer <> [
                class' "ui-board dessin-board" true,
                -- todo
                contextmenu' \ev -> preventDefault ev *> playA Nothing]) [
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
                    click $ playA (Just i)
                ],
            [maybeN $ currentLine <$> (state^._pointer) <*> (getCoords graph <$> join (last position))]
        ],
        span [class' "dessin-raise-info dessin-raise-info" true] [
            text $ show raises <> " levé" <> s <> " de crayon"
        ],
        button [
            class' "ui-button ui-button-primary dessin-raise" true,
            disabled $ not (canPlay state Nothing),
            click $ playA Nothing
        ] [text "Lever le crayon"]
    ]

    rules = [
        text "blah blah blah blah blah blah blah blah", br,
        text "blah blah blah blah blah blah blah blah"
    ]

    winTitle = "Tu as réussi en " <> show raises <> " levé" <> s