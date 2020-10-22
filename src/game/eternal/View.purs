module Game.Eternal.View (view) where

import MyPrelude

import Math (acos)
import Game.Common (pointerDecoder)
import Game.Core (CoreMsg(SetPointer), isLevelFinished, PointerPosition, core, _position, _pointer)
import Game.Eternal.Model (State, Msg(..), Graph, Phase(..), Rules(..), GraphKind(..), Pos, Edge, (↔), isValidNextMove,
                            _graph, _phase, _graphkind, _draggedGuard, _rules, _nextmove)
import Pha as H
import Pha.Elements as HH
import Pha.Attributes as P
import Pha.Events as E
import Pha.Util (translate, pc)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup, icons2Players, iundo, iredo, ireset, iclear, irules)
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

cursor ∷ ∀a b. PointerPosition → b → H.VDom a
cursor pp _ = HH.use $ 
                [   H.key "cursor"
                ,   P.href "#roman"
                ,   P.width "6"
                ,   P.height "12"
                ,   P.x "-3"
                ,   P.y "-6"
                ,   H.style "pointer-events" "none"
                ] <> svgCursorStyle pp

-- les fonctions dndBoardProps et dndItemProps de Game.Core ne sont pas assez génériques pour Eternal
-- todo: refactoriser
dndBoardProps ∷ Array (H.Prop Msg)
dndBoardProps =
    [   E.on "pointerdown" move
    ,   E.on "pointermove" move
    ,   E.onpointerup DropOnBoard
    ,   E.onpointerleave LeaveGuard
    ] where
        move e = map (core <<< SetPointer <<< Just) <$> pointerDecoder e

dndItemProps ∷ State → 
    {
        draggable ∷ Boolean,
        droppable ∷ Boolean,
        id ∷ Int,
        currentDragged ∷ Maybe Int
    } → Array (H.Prop Msg)
dndItemProps state {draggable, droppable, id, currentDragged} =
    [   H.class' "draggable" draggable
    ,   H.class' "dragged" dragged
    ,   H.class' "candrop" candrop
    ,   E.releasePointerCaptureOn "pointerdown" $ E.always (if draggable then Just (DragGuard id) else Nothing)
    ,   E.stopPropagationOn "pointerup" $ E.always (if candrop then Just (DropGuard id) ∧ true else Nothing ∧ false)
    ] where
        candrop = droppable && isJust currentDragged
        dragged = draggable && Just id == currentDragged


drawArrow ∷ ∀a. Number → Number → Number → Number → H.VDom a
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
    in HH.g 
        []
        [   HH.line 
            [   -- key?
                P.x1 $ show x1
            ,   P.y1 $ show y1
            ,   P.x2 $ show x2
            ,   P.y2 $ show y2
            ,   H.class_ "dessin-line2"
            ]
        ,   HH.path [P.d arrowPath, P.fill "red"]
        ]


view ∷ State → H.VDom Msg
view state = template {config, board, rules, winTitle} state where
    position = state^._position
    graph = state^._graph
    guards = (state^._position).guards
    grules = state^._rules
    phase = state^._phase

    config =    
        card "Domination éternelle" 
        [   iconSelectGroup state "Type de graphe" [Path, Cycle, Biclique, Sun, Grid] (state^._graphkind) SetGraphKind (case _ of 
                Path → _{icon = IconSymbol "#graph-path", tooltip = Just "Chemin" }
                Cycle → _{icon = IconSymbol "#graph-cycle", tooltip = Just "Cycle" }
                Biclique → _{icon = IconSymbol "#graph-biclique", tooltip = Just "Biclique" }
                Grid → _{icon = IconSymbol "#graph-grid", tooltip = Just "Grille" }
                Sun → _{icon = IconSymbol "#graph-sun", tooltip = Just "Soleil" }
            )
        ,   iconSelectGroup state "Règles" [OneGuard, ManyGuards] grules SetRules (case _ of 
                OneGuard → _{icon = IconText "1", tooltip = Just "Un seul garde" }
                ManyGuards → _{icon = IconText "∞", tooltip = Just "Plusieurs gardes" }
            )
        ,   icons2Players state
        ,   icongroup "Options" $ [iundo, iredo, ireset, iclear, irules] <#> (_ $ state)
        ]

    grid =
        HH.div (dndBoardProps <>
            [   H.class_ "ui-board eternal-board", H.style "width" "100%", H.style "height" "100%"
            ])
            [   HH.svg [H.class_ "eternal-svg", P.viewBox 0 0 100 100]
                [   HH.g [] $
                        graph.edges <#> \edge →
                            H.maybe (getCoordsOfEdge graph edge) \{x1, x2, y1, y2} →
                                HH.line 
                                [   -- key?
                                    P.x1 $ show (100.0 * x1)
                                ,   P.y1 $ show (100.0 * y1)
                                ,   P.x2 $ show (100.0 * x2)
                                ,   P.y2 $ show (100.0 * y2)
                                ,   H.class_ "dessin-line1"
                                ]
                ,   H.when (grules == ManyGuards) \_ →
                        HH.g [] $
                            (zip guards (state^._nextmove)) <#> \(from ∧ to) →
                                H.when (from /= to) \_ →
                                    H.maybe (getCoordsOfEdge graph (from ↔ to)) \{x1, x2, y1, y2} →
                                        drawArrow (x1 * 100.0) (x2 * 100.0) (y1 * 100.0) (y2 * 100.0)
                ,   HH.g [] $
                        graph.vertices # mapWithIndex \i {x, y} →
                            HH.circle $
                            [   H.key $ show i
                            ,   P.cx $ show (100.0 * x)
                            ,   P.cy $ show (100.0 * y)
                            ,   P.r "3"
                            ,   P.fill "blue"
                            ]
                ,   HH.g [] $
                        guards # mapWithIndex \i index →
                            HH.use
                            [   H.key $ show i
                            ,   P.href "#roman"
                            ,   P.width "6"
                            ,   P.height "12"
                            ,   P.x "-3"
                            ,   P.y "-6"
                            ,   H.class_ "eternal-guard"
                            ,   H.class' "no-move" (phase == PrepPhase)
                            ,   H.style "transform" $ fromMaybe "none" (translateGuard <$> getCoords graph index)
                            ]
                ,   H.maybe (position.attacked) \attack →
                        HH.use
                        [   H.key "attack"
                        ,   P.href "#eternal-attack"
                        ,   P.width "8"
                        ,   P.height "8"
                        ,   P.x "-4"
                        ,   P.y "-4"
                        ,   H.style "transform" $ fromMaybe "none" (translateGuard <$> getCoords graph attack)
                        ,   H.style "pointer-events" "none"
                        ]
                ,   H.maybeN $ cursor <$> state^._pointer <*> state^._draggedGuard
                ,   HH.g [] $ 
                        graph.vertices # mapWithIndex \i pos →
                            HH.rect $
                            [   H.key $ show i
                            ,   P.width "10"
                            ,   P.height "10"
                            ,   P.x "-5"
                            ,   P.y "-5"
                            ,   P.fill "transparent"
                            ,   H.style "transform" $ translateGuard pos
                            ,   E.onclick' $ if grules == ManyGuards && isJust position.attacked then Nothing else Just (Play i)
                            ,   H.class' "eternal-sel" $ phase == PrepPhase || isJust position.attacked && (grules == ManyGuards) == elem i guards 
                                                                          || not (isJust position.attacked) && not (elem i guards)
                            ]  <> (dndItemProps state
                                {   draggable: grules == ManyGuards && isJust position.attacked && elem i guards
                                ,   droppable: true
                                ,   id: i
                                ,   currentDragged: state^._draggedGuard
                                }
                            )
                ]
            ,   HH.span [H.class_ "eternal-info"] [
                    H.text (
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
            ,   HH.button
                [   H.class_ "ui-button ui-button-primary dessin-raise"
                ,   P.disabled $ null position.guards || state^._phase == GamePhase && (state^._rules == OneGuard || isNothing position.attacked || not (isValidNextMove state (state^._nextmove)))
                ,   E.onclick (if state^._phase == GamePhase then MoveGuards else StartGame)
                ]
                [   H.text "Valider"]
            ]

    board = incDecGrid state [grid]

    rules = 
        [   H.text "Domination Eternelle est un jeu à deux joueurs: un attaquant et un défenseur."
        ,   HH.br
        ,   H.text "Au début de la partie, le défenseur choisit des sommets sur lesquels poser des gardes."
        ,   HH.br
        ,   H.text "Ensuite, à chaque tour, l'attaquant choisit d'attaquer un sommet puis le défenseur doit déplacer un de ses gardes"
        ,   H.text " vers le sommet attaqué à la condition que celui soit adjacent au garde."
        ,   HH.br
        ,   H.text "Si le défenseur ne peut pas déplacer de garde, il perd la partie."
        ,   HH.br
        ,   H.text "La partie peut ne pas avoir de fin. Le but est de déterminer le nombre minimum de gardes pour défendre infiniment toute attaque."
        ,   HH.br
        ,   H.text "Dans une variante, le défenseur peut déplacer plusieurs gardes à chaque tour."
        ]

    winTitle = "L'attaquant gagne"