module Game.Eternal.View (view) where
import MyPrelude
import Data.Unfoldable as Unfoldable
import Pha (VDom, key, text, maybeN, (<??>), class_, style)
import Pha.Elements (div, button, span, br)
import Pha.Events (onclick, oncontextmenu)
import Pha.Attributes (disabled)
import Pha.Svg (svg, line, circle, viewBox, use, stroke, fill, width, height, x_, y_, x1, x2, y1, y2, cx, cy, r)
import Pha.Util (translate, pc)
import Game.Core (canPlay, _position, _pointer)
import Game.Eternal.Model (State, Msg(..), Graph, Pos, Edge, (↔), _graph)
import UI.Template (template, card, trackPointer)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)

getCoords ∷ Graph → Int → Maybe Pos
getCoords graph u = graph.vertices !! u

getCoordsOfEdge ∷ Graph → Edge → Maybe {px1 ∷ Number, px2 ∷ Number, py1 ∷ Number, py2 ∷ Number}
getCoordsOfEdge graph (u ↔ v) = do
    {x: px1, y: py1} ← getCoords graph u
    {x: px2, y: py2} ← getCoords graph v
    pure {px1, px2, py1, py2}

translateGuard ∷ Pos → String
translateGuard {x, y} = translate (pc x) (pc y)

{-
dndItemProps ∷ State → 
    {
        draggable ∷ Boolean,
        droppable ∷ Boolean,
        id ∷ Int,
        currentDragged ∷ Maybe Int
    } → Array (Prop msg)
dndItemProps state {draggable, droppable, id, currentDragged} =
    [   class' "dragged" dragged
    ,   class' "candrop" candrop
    ,   releasePointerCaptureOn "pointerdown" $ always (if draggable then Just $ dndmsg (Drag id) else Nothing)
    ,   onpointerup' $ if candrop then Just PrepareMove id else Nothing
    ] where
        candrop = droppable && (currentDragged # maybe false \d → canMove state d id)
        dragged = draggable && Just id == currentDragged

-}

view ∷ State → VDom Msg
view state = template {config, board, rules, winTitle} state where
    position = state^._position
    graph = state^._graph
    guards = (state^._position).guards

    config =    
        card "Eternal" 
        []

    board =
        div (
            [   class_ "ui-board eternal-board"
            ])
            [   svg [class_ "eternal-svg", viewBox 0 0 100 100] $ concat 
                [   graph.edges <#> \edge →
                    getCoordsOfEdge graph edge <??> \{px1, px2, py1, py2} →
                        line 
                        [   x1 $ show (100.0 * px1)
                        ,   y1 $ show (100.0 * py1)
                        ,   x2 $ show (100.0 * px2)
                        ,   y2 $ show (100.0 * py2)
                        ,   class_ "dessin-line1"
                        ]
                ,   graph.vertices # mapWithIndex \i {x, y} →
                        circle
                        [   cx $ show (100.0 * x)
                        ,   cy $ show (100.0 * y)
                        ,   r "3"
                        ,   fill "blue"
                        ,   onclick $ Play i
                        ]
                ,   guards # mapWithIndex \i index →
                    use "#firetruck"
                        [   key $ show i
                        ,   width "8"
                        ,   height "8"
                        ,   x_ "-4"
                        ,   y_ "-4"
                        ,   class_ "eternal-guard"
                        ,   style "transform" $ fromMaybe "none" (translateGuard <$> getCoords graph index)
                        ]
                ,   Unfoldable.fromMaybe position.attacked <#> \attack →
                    use "#eternal-attack"
                        [   key "attack"
                        ,   width "8"
                        ,   height "8"
                        ,   x_ "-4"
                        ,   y_ "-4"
                        ,   style "transform" $ fromMaybe "none" (translateGuard <$> getCoords graph attack)
                        ]
                ]
            ]

    rules = 
        [   text "Le but du jeu est de dessiner le motif indiqué en pointillé en levant le moins souvent possible le crayon."
        ,   br
        ,   text "Pour lever le crayon, tu peux cliquer sur le bouton prévu pour ou utiliser le clic droit."
        ]

    winTitle = ""