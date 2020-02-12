module Game.Eternal.View (view) where

import MyPrelude

import Game.Common (pointerDecoder)
import Game.Core (CoreMsg(SetPointer), isLevelFinished, PointerPosition, core, _position, _pointer)
import Game.Eternal.Model (State, Msg(..), Graph, Phase(..), Rules(..), GraphKind(..), Pos, Edge, (↔), isValidNextMove, _graph, _phase, _graphkind, _draggedGuard, _rules, _nextmove)
import Pha (VDom, Prop, key, text, maybeN, (<&&>), (<??>), class_, class', style)
import Pha.Attributes (disabled)
import Pha.Elements (div, button, span, br)
import Pha.Events (on, onclick, onclick', releasePointerCaptureOn, stopPropagationOn, onpointerup, onpointerleave)
import Pha.Events.Decoder (always)
import Pha.Svg (svg, g, line, circle, viewBox, use, fill, width, height, x_, y_, x1, x2, y1, y2, cx, cy, r)
import Pha.Util (translate, pc)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)
import UI.Template (template, card, incDecGrid, svgCursorStyle)


getCoords ∷ Graph → Int → Maybe Pos
getCoords graph u = graph.vertices !! u

getCoordsOfEdge ∷ Graph → Edge → Maybe {px1 ∷ Number, px2 ∷ Number, py1 ∷ Number, py2 ∷ Number}
getCoordsOfEdge graph (u ↔ v) = do
    {x: px1, y: py1} ← getCoords graph u
    {x: px2, y: py2} ← getCoords graph v
    pure {px1, px2, py1, py2}

translateGuard ∷ Pos → String
translateGuard {x, y} = translate (pc x) (pc y)

cursor ∷ ∀a b. PointerPosition → b → VDom a
cursor pp _ = use "#roman" $
                [   key "cursor"
                ,   width "6"
                ,   height "12"
                ,   x_ "-3"
                ,   y_ "-6"
                ,   style "pointer-events" "none"
                ] <> svgCursorStyle pp

-- les fonctions dndBoardProps et dndItemProps de Game.Core ne sont pas assez génériques pour Eternal
-- todo: refactoriser
dndBoardProps ∷ Array (Prop Msg)
dndBoardProps =
    [   on "pointerdown" move
    ,   on "pointermove" move
    ,   onpointerup DropOnBoard
    ,   onpointerleave LeaveGuard
    ] where
        move e = core <$> (SetPointer <$> Just <$> pointerDecoder e)

dndItemProps ∷ State → 
    {
        draggable ∷ Boolean,
        droppable ∷ Boolean,
        id ∷ Int,
        currentDragged ∷ Maybe Int
    } → Array (Prop Msg)
dndItemProps state {draggable, droppable, id, currentDragged} =
    [   class' "draggable" draggable
    ,   class' "dragged" dragged
    ,   class' "candrop" candrop
    ,   releasePointerCaptureOn "pointerdown" $ always (if draggable then Just (DragGuard id) else Nothing)
    ,   stopPropagationOn "pointerup" $ always (if candrop then Just (DropGuard id) /\ true else Nothing /\ false)
    ] where
        candrop = droppable && isJust currentDragged
        dragged = draggable && Just id == currentDragged

view ∷ State → VDom Msg
view state = template {config, board, rules, winTitle} state where
    position = state^._position
    graph = state^._graph
    guards = (state^._position).guards
    grules = state^._rules

    config =    
        card "Domination éternelle" 
        [   iconSelectGroup state "Type de graphe" [Path, Cycle] (state^._graphkind) SetGraphKind (case _ of 
                Path → _{icon = IconText "P", tooltip = Just "Chemin" }
                Cycle → _{icon = IconText "C", tooltip = Just "Cycle" }
            )
        ,   iconSelectGroup state "Règles" [OneGuard, ManyGuards] grules SetRules (case _ of 
                OneGuard → _{icon = IconText "1", tooltip = Just "Un seul garde" }
                ManyGuards → _{icon = IconText "∞", tooltip = Just "Plusieurs gardes" }
            )
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> \x → x state
        ]

    grid =
        div (dndBoardProps <>
            [   class_ "ui-board eternal-board", style "width" "100%", style "height" "100%"
            ])
            [   svg [class_ "eternal-svg", viewBox 0 0 100 100]
                [   g [] $
                        graph.edges <#> \edge →
                            getCoordsOfEdge graph edge <??> \{px1, px2, py1, py2} →
                                line 
                                [   -- key?
                                    x1 $ show (100.0 * px1)
                                ,   y1 $ show (100.0 * py1)
                                ,   x2 $ show (100.0 * px2)
                                ,   y2 $ show (100.0 * py2)
                                ,   class_ "dessin-line1"
                                ]
                ,   grules == ManyGuards <&&> \_ ->
                        g [] $  ----- todo
                            (zip guards (state^._nextmove)) <#> \(from /\ to) →
                                getCoordsOfEdge graph (from ↔ to) <??> \{px1, px2, py1, py2} →
                                    line 
                                    [   -- key?
                                        x1 $ show (100.0 * px1)
                                    ,   y1 $ show (100.0 * py1)
                                    ,   x2 $ show (100.0 * px2)
                                    ,   y2 $ show (100.0 * py2)
                                    ,   class_ "dessin-line2"
                                    ]
                ,   g [] $ 
                        graph.vertices # mapWithIndex \i {x, y} →
                            circle $
                            [   key $ "v" <> show i
                            ,   cx $ show (100.0 * x)
                            ,   cy $ show (100.0 * y)
                            ,   r "3"
                            ,   fill "blue"
                            ,   onclick' $ if grules == ManyGuards && isJust position.attacked then Nothing else Just (Play i)
                            ]  <> (dndItemProps state
                                {   draggable: grules == ManyGuards && isJust position.attacked
                                ,   droppable: true
                                ,   id: i
                                ,   currentDragged: state^._draggedGuard
                                }
                            )
                ,   g [] $
                        guards # mapWithIndex \i index →
                            use "#roman" $
                            [   key $ "g" <> show i
                            ,   width "6"
                            ,   height "12"
                            ,   x_ "-3"
                            ,   y_ "-6"
                            ,   class_ "eternal-guard"
                            ,   style "transform" $ fromMaybe "none" (translateGuard <$> getCoords graph index)
                            ] {- <> (dndItemProps state
                                {   draggable: grules == ManyGuards && isJust position.attacked
                                ,   droppable: true
                                ,   id: index
                                ,   currentDragged: state^._draggedGuard
                                }
                            )
                            -}
                ,   maybeN $ position.attacked <#> \attack →
                        use "#eternal-attack"
                        [   key "attack"
                        ,   width "8"
                        ,   height "8"
                        ,   x_ "-4"
                        ,   y_ "-4"
                        ,   style "transform" $ fromMaybe "none" (translateGuard <$> getCoords graph attack)
                        ,   style "pointer-events" "none"
                        ]
                ,   maybeN $ cursor <$> state^._pointer <*> state^._draggedGuard
                ]
            ,   span [class_ "eternal-info"] [
                    text (
                        if isLevelFinished state then
                            "Le sommet attaqué ne peut être défendu"
                        else if state^._phase == PrepPhase then
                            "Choisis la position initiale des gardes"
                        else if isJust (position.attacked) then
                            "Déplace un garde vers le sommet attaqué"
                        else
                            "Choisis un sommet à attaquer"
                    )
                ]
            ,   button
                [   class_ "ui-button ui-button-primary dessin-raise"
                ,   disabled $ state^._phase == GamePhase  && (state^._rules == OneGuard || isNothing position.attacked || not (isValidNextMove state (state^._nextmove)))
                ,   onclick (if state^._phase == GamePhase then MoveGuards else StartGame)
                ]
                [   text "Valider"]
            ]

    board = incDecGrid state [grid]

    rules = 
        [   text "Le but du jeu est de dessiner le motif indiqué en pointillé en levant le moins souvent possible le crayon."
        ,   br
        ,   text "Pour lever le crayon, tu peux cliquer sur le bouton prévu pour ou utiliser le clic droit."
        ]

    winTitle = "Perdu"