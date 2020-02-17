module Game.Eternal.View (view) where

import MyPrelude

import Math (acos)
import Game.Common (pointerDecoder)
import Game.Core (CoreMsg(SetPointer), isLevelFinished, PointerPosition, core, _position, _pointer)
import Game.Eternal.Model (State, Msg(..), Graph, Phase(..), Rules(..), GraphKind(..), Pos, Edge, (↔), isValidNextMove,
                            _graph, _phase, _graphkind, _draggedGuard, _rules, _nextmove)
import Pha (VDom, Prop, key, text, maybeN, (<&&>), (<??>), class_, class', style)
import Pha.Attributes (href, disabled)
import Pha.Elements (br, button, div, span)
import Pha.Events (on, onclick, onclick', releasePointerCaptureOn, stopPropagationOn, onpointerup, onpointerleave)
import Pha.Events.Decoder (always)
import Pha.Svg (svg, g, line, rect, circle, viewBox, use, path,
                fill, width, height, d_, x_, y_, x1_, x2_, y1_, y2_, cx, cy, r_)
import Pha.Util (translate, pc)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup, icons2Players, iundo, iredo, ireset, irules)
import UI.Template (template, card, incDecGrid, svgCursorStyle)


getCoords ∷ Graph → Int → Maybe Pos
getCoords graph u = graph.vertices !! u

getCoordsOfEdge ∷ Graph → Edge → Maybe {x1 ∷ Number, x2 ∷ Number, y1 ∷ Number, y2 ∷ Number}
getCoordsOfEdge graph (u ↔ v) = do
    {x: x1, y: y1} ← getCoords graph u
    {x: x2, y: y2} ← getCoords graph v
    pure {x1, x2, y1, y2}

translateGuard ∷ Pos → String
translateGuard {x, y} = translate (pc x) (pc y)

cursor ∷ ∀a b. PointerPosition → b → VDom a
cursor pp _ = use $ 
                [   key "cursor"
                ,   href "#roman"
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


drawArrow ∷ ∀a. Number → Number → Number → Number → VDom a
drawArrow x1 x2 y1 y2 =
    let arrowSize = 6.0
        dx = x2 - x1
        dy = y2 - y1
        len = sqrt (dx*dx + dy*dy)
        angle' = acos (dx / len)
        angle = if dy >= 0.0 then 2.0 * pi - angle' else angle'
        x3 = x2 + arrowSize * sin (angle - pi / 3.0)
        y3 = y2 + arrowSize * cos (angle - pi / 3.0)
        x4 = x2 + arrowSize * sin (angle - 2.0 * pi / 3.0)
        y4 = y2 + arrowSize * cos (angle - 2.0 * pi / 3.0)
        arrowPath = "M" <> show x2 <> "," <> show y2 
                    <> "L" <> show x3 <> "," <> show y3
                    <> "L" <> show x4 <> "," <> show y4 <> "z"
    in g 
        []
        [   line 
            [   -- key?
                x1_ $ show x1
            ,   y1_ $ show y1
            ,   x2_ $ show x2
            ,   y2_ $ show y2
            ,   class_ "dessin-line2"
            ]
        ,   path [d_ arrowPath, fill "red"]
        ]


view ∷ State → VDom Msg
view state = template {config, board, rules, winTitle} state where
    position = state^._position
    graph = state^._graph
    guards = (state^._position).guards
    grules = state^._rules
    phase = state^._phase

    config =    
        card "Domination éternelle" 
        [   iconSelectGroup state "Type de graphe" [Path, Cycle, Biclique, Sun, Grid] (state^._graphkind) SetGraphKind (case _ of 
                Path → _{icon = IconText "P", tooltip = Just "Chemin" }
                Cycle → _{icon = IconText "C", tooltip = Just "Cycle" }
                Biclique → _{icon = IconText "*", tooltip = Just "Biclique" }
                Grid → _{icon = IconText "G", tooltip = Just "Grille" }
                Sun → _{icon = IconText "S", tooltip = Just "Soleil" }
            )
        ,   iconSelectGroup state "Règles" [OneGuard, ManyGuards] grules SetRules (case _ of 
                OneGuard → _{icon = IconText "1", tooltip = Just "Un seul garde" }
                ManyGuards → _{icon = IconText "∞", tooltip = Just "Plusieurs gardes" }
            )
        ,   icons2Players state
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> \x → x state
        ]

    grid =
        div (dndBoardProps <>
            [   class_ "ui-board eternal-board", style "width" "100%", style "height" "100%"
            ])
            [   svg [class_ "eternal-svg", viewBox 0 0 100 100]
                [   g [] $
                        graph.edges <#> \edge →
                            getCoordsOfEdge graph edge <??> \{x1, x2, y1, y2} →
                                line 
                                [   -- key?
                                    x1_ $ show (100.0 * x1)
                                ,   y1_ $ show (100.0 * y1)
                                ,   x2_ $ show (100.0 * x2)
                                ,   y2_ $ show (100.0 * y2)
                                ,   class_ "dessin-line1"
                                ]
                ,   grules == ManyGuards <&&> \_ →
                        g [] $  ----- todo
                            (zip guards (state^._nextmove)) <#> \(from /\ to) →
                                from /= to <&&> \_ →
                                    getCoordsOfEdge graph (from ↔ to) <??> \{x1, x2, y1, y2} →
                                        drawArrow (x1 * 100.0) (x2 * 100.0) (y1 * 100.0) (y2 * 100.0)
                ,   g [] $ 
                        graph.vertices # mapWithIndex \i {x, y} →
                            circle $
                            [   key $ show i
                            ,   cx $ show (100.0 * x)
                            ,   cy $ show (100.0 * y)
                            ,   r_ "3"
                            ,   fill "blue"
                            ]
                ,   g [] $
                        guards # mapWithIndex \i index →
                            use
                            [   key $ show i
                            ,   href "#roman"
                            ,   width "6"
                            ,   height "12"
                            ,   x_ "-3"
                            ,   y_ "-6"
                            ,   class_ "eternal-guard"
                            ,   class' "no-move" (phase == PrepPhase)
                            ,   style "transform" $ fromMaybe "none" (translateGuard <$> getCoords graph index)
                            ]
                ,   maybeN $ position.attacked <#> \attack →
                        use 
                        [   key "attack"
                        ,   href "#eternal-attack"
                        ,   width "8"
                        ,   height "8"
                        ,   x_ "-4"
                        ,   y_ "-4"
                        ,   style "transform" $ fromMaybe "none" (translateGuard <$> getCoords graph attack)
                        ,   style "pointer-events" "none"
                        ]
                ,   maybeN $ cursor <$> state^._pointer <*> state^._draggedGuard
                ,   g [] $ 
                        graph.vertices # mapWithIndex \i pos →
                            rect $
                            [   key $ show i
                            ,   width "10"
                            ,   height "10"
                            ,   x_ "-5"
                            ,   y_ "-5"
                            ,   fill "transparent"
                            ,   style "transform" $ translateGuard pos
                            ,   onclick' $ if grules == ManyGuards && isJust position.attacked then Nothing else Just (Play i)
                            ]  <> (dndItemProps state
                                {   draggable: grules == ManyGuards && isJust position.attacked && elem i guards
                                ,   droppable: true
                                ,   id: i
                                ,   currentDragged: state^._draggedGuard
                                }
                            )
                ]
            ,   span [class_ "eternal-info"] [
                    text (
                        if isLevelFinished state then
                            "Le sommet attaqué ne peut être défendu"
                        else if phase == PrepPhase then
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
        [   text "Domination Eternelle est un jeu à deux joueurs: un attaquant et un défenseur."
        ,   br
        ,   text "Au début de la partie, le défenseur choisit des sommets sur lesquels poser des gardes."
        ,   br
        ,   text "Ensuite, à chaque tour, l'attaquant choisit d'attaquer un sommet puis le défenseur doit déplacer un de ses gardes"
        ,   text " vers le sommet attaqué à la condition que celui soit adjacent au garde."
        ,   br
        ,   text "Si le défenseur ne peut pas déplacer de garde, il perd la partie."
        ,   br
        ,   text "La partie peut ne pas avoir de fin. Le but est de déterminer le nombre minimum de gardes pour défendre infiniment toute attaque."
        ,   br
        ,   text "Dans une variante, le défenseur peut déplacer plusieurs gardes à chaque tour."
        ]

    winTitle = "L'attaquant gagne"