module Game.Eternal.View (view) where

import MamPrelude

import Data.Number (acos)
import Game.Core (CoreMsg(SetPointer), isLevelFinished, PointerPosition, core, _position, _pointer)
import Game.Eternal.Model (Model, Msg(..), Phase(..), Rules(..), GraphKind(..), isValidNextMove, _graph, _phase, _graphkind, _draggedGuard, _rules, _nextmove, _geditor)
import Game.Helpers (pointerDecoder, releasePointerCapture)
import Lib.Graph (Graph, Edge, (↔), Position)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (translate, pc)
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import UI.GraphEditor as GEditor
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup', icons2Players, iundo, iredo, ireset, iclear, irules)
import UI.Template (template, card, incDecGrid, svgCursorStyle)
import Web.PointerEvent.PointerEvent as PE
import Web.Event.Event (stopPropagation)

getCoords ∷ Graph → Int → Maybe Position
getCoords graph u = graph.vertices !! u

getCoordsOfEdge ∷ Graph → Edge → Maybe { x1 ∷ Number, x2 ∷ Number, y1 ∷ Number, y2 ∷ Number }
getCoordsOfEdge graph (u ↔ v) = do
  { x: x1, y: y1 } ← getCoords graph u
  { x: x2, y: y2 } ← getCoords graph v
  pure { x1, x2, y1, y2 }

translateGuard ∷ Position → String
translateGuard { x, y } = translate (pc x) (pc y)

cursor ∷ ∀ a b. PointerPosition → b → Html a
cursor pp _ = S.use
  $
    [ P.href "#roman"
    , SA.width 6
    , SA.height 12
    , SA.x (-3.0)
    , SA.y (-6.0)
    , H.style "pointer-events" "none"
    ]
  <> svgCursorStyle pp

-- les fonctions dndBoardProps et dndItemProps de Game.Core ne sont pas assez génériques pour Eternal
-- todo: refactoriser
dndBoardProps ∷ Array (H.Prop Msg)
dndBoardProps =
  [ E.onPointerDown' $ pointerDecoder (core <<< SetPointer) <<< PE.toMouseEvent
  , E.onPointerMove' $ pointerDecoder (core <<< SetPointer) <<< PE.toMouseEvent
  , E.onPointerUp \_ → DropOnBoard
  , E.onPointerLeave \_ → LeaveGuard
  ]

dndItemProps
  ∷ Model
  → { draggable ∷ Boolean
    , droppable ∷ Boolean
    , id ∷ Int
    , currentDragged ∷ Maybe Int
    }
  → Array (H.Prop Msg)
dndItemProps _ { draggable, droppable, id, currentDragged } =
  [ H.class' "draggable" draggable
  , H.class' "dragged" dragged
  , H.class' "candrop" candrop
  , E.onPointerDown' \ev →
      if draggable then do
        releasePointerCapture ev
        pure $ Just $ DragGuard id
      else
        pure Nothing
  , E.onPointerUp' \ev →
      if candrop then do
        stopPropagation $ PE.toEvent ev
        pure $ Just $ DropGuard id
      else
        pure Nothing
  ]
  where
  candrop = droppable && isJust currentDragged
  dragged = draggable && Just id == currentDragged

drawArrow ∷ ∀ a. Number → Number → Number → Number → Html a
drawArrow x1 x2 y1 y2 =
  let
    arrowSize = 6.0
    dx = x2 - x1
    dy = y2 - y1
    len = sqrt (dx * dx + dy * dy)
    angle' = acos (dx / len)
    angle = if dy >= 0.0 then 2.0 * pi - angle' else angle'
    x3 = x2 + arrowSize * sin (angle - pi / 3.0)
    y3 = y2 + arrowSize * cos (angle - pi / 3.0)
    x4 = x2 + arrowSize * sin (angle - 2.0 * pi / 3.0)
    y4 = y2 + arrowSize * cos (angle - 2.0 * pi / 3.0)
    arrowPath = "M" <> show x2 <> "," <> show y2
      <> "L"
      <> show x3
      <> ","
      <> show y3
      <> "L"
      <> show x4
      <> ","
      <> show y4
      <> "z"
  in
    S.g
      []
      [ S.line
          [ SA.x1 x1
          , SA.y1 y1
          , SA.x2 x2
          , SA.y2 y2
          , H.class_ "dessin-line2"
          ]
      , S.path [ SA.d arrowPath, SA.fill "red" ]
      ]

view ∷ Model → Html Msg
view model = template { config, board, rules, winTitle, customDialog } model
  where
  position = model ^. _position
  graph = model ^. _graph
  guards = (model ^. _position).guards
  grules = model ^. _rules
  phase = model ^. _phase

  config =
    card "Domination éternelle"
      [ iconSelectGroup' model "Type de graphe" (model ^. _graphkind) SetGraphKind
          [ Path ∧ _ { icon = IconSymbol "#graph-path", tooltip = Just "Chemin" }
          , Cycle ∧ _ { icon = IconSymbol "#graph-cycle", tooltip = Just "Cycle" }
          , Biclique ∧ _ { icon = IconSymbol "#graph-biclique", tooltip = Just "Biclique" }
          , Grid ∧ _ { icon = IconSymbol "#graph-grid", tooltip = Just "Grille" }
          , CustomGraph ∧ _ { icon = IconSymbol "#customize", tooltip = Just "Crée ton graphe" }
          ]
      , iconSelectGroup' model "Règles" grules SetRules
          [ OneGuard ∧ _ { icon = IconText "1", tooltip = Just "Un seul garde" }
          , ManyGuards ∧ _ { icon = IconText "∞", tooltip = Just "Plusieurs gardes" }
          ]
      , icons2Players model
      , icongroup "Options" $ [ iundo, iredo, ireset, iclear, irules ] # map (_ $ model)
      ]

  grid =
    H.div
      ( dndBoardProps <>
          [ H.class_ "ui-board eternal-board"
          , H.style "width" "100%"
          , H.style "height" "100%"
          ]
      )
      [ S.svg [ H.class_ "eternal-svg", SA.viewBox 0.0 0.0 100.0 100.0 ]
          [ S.g []
              $ graph.edges
              # map \edge →
                H.maybe (getCoordsOfEdge graph edge) \{ x1, x2, y1, y2 } →
                  S.line
                    [ -- key?
                      SA.x1 $ 100.0 * x1
                    , SA.y1 $ 100.0 * y1
                    , SA.x2 $ 100.0 * x2
                    , SA.y2 $ 100.0 * y2
                    , H.class_ "dessin-line1"
                    ]
          , H.when (grules == ManyGuards) \_ →
              S.g []
                $ (zip guards (model ^. _nextmove))
                # map \(from ∧ to) →
                  H.when (from ≠ to) \_ →
                    H.maybe (getCoordsOfEdge graph (from ↔ to)) \{ x1, x2, y1, y2 } →
                      drawArrow (x1 * 100.0) (x2 * 100.0) (y1 * 100.0) (y2 * 100.0)
          , S.g []
              $ graph.vertices
              # map \{ x, y } →
                S.circle $
                  [ SA.cx $ 100.0 * x
                  , SA.cy $ 100.0 * y
                  , SA.r 3.0
                  , SA.fill "blue"
                  ]
          , S.g []
              $ guards
              # map \index →
                S.use
                  [ P.href "#roman"
                  , SA.width 6
                  , SA.height 12
                  , SA.x (-3)
                  , SA.y (-6)
                  , H.class_ "eternal-guard"
                  , H.class' "no-move" (phase == PrepPhase)
                  , H.style "transform" $ fromMaybe "none" (translateGuard <$> getCoords graph index)
                  ]
          , H.maybe (position.attacked) \attack →
              S.use
                [ P.href "#eternal-attack"
                , SA.width 8
                , SA.height 8
                , SA.x (-4)
                , SA.y (-4)
                , H.style "transform" $ fromMaybe "none" (translateGuard <$> getCoords graph attack)
                , H.style "pointer-events" "none"
                ]
          , H.fromMaybe $ cursor <$> model ^. _pointer <*> model ^. _draggedGuard
          , S.g []
              $ graph.vertices
              # mapWithIndex \i pos →
                  S.rect
                    $
                      [ SA.width 10
                      , SA.height 10
                      , SA.x (-5)
                      , SA.y (-5)
                      , SA.fill "transparent"
                      , H.style "transform" $ translateGuard pos
                      , E.onClick \_ → if grules == ManyGuards && isJust position.attacked then NoAction else Play i
                      , H.class' "eternal-sel" $ phase == PrepPhase || isJust position.attacked && (grules == ManyGuards) == elem i guards
                          || not (isJust position.attacked)
                          && not (elem i guards)
                      ]
                    <>
                      ( dndItemProps model
                          { draggable: grules == ManyGuards && isJust position.attacked && elem i guards
                          , droppable: true
                          , id: i
                          , currentDragged: model ^. _draggedGuard
                          }
                      )
          ]
      , H.span [ H.class_ "eternal-info" ]
          [ H.text
              ( if isLevelFinished model then
                  "Le sommet attaqué ne peut être défendu"
                else if phase == PrepPhase then
                  "Choisis la position initiale des gardes"
                else if isJust (position.attacked) then
                  "Déplace un garde vers le sommet attaqué"
                else
                  "Choisis un sommet à attaquer"
              )
          ]
      , H.button
          [ H.class_ "ui-button ui-button-primary dessin-raise"
          , P.disabled $ null position.guards || model ^. _phase == GamePhase && (model ^. _rules == OneGuard || isNothing position.attacked || not (isValidNextMove model (model ^. _nextmove)))
          , E.onClick \_ → if model ^. _phase == GamePhase then MoveGuards else StartGame
          ]
          [ H.text "Valider" ]
      ]

  board = incDecGrid model [ grid ]

  customDialog _ = GEditor.view (model ^. _geditor) CloseEditor

  rules =
    [ H.text "Domination Eternelle est un jeu à deux joueurs: un attaquant et un défenseur."
    , H.br
    , H.text "Au début de la partie, le défenseur choisit des sommets sur lesquels poser des gardes."
    , H.br
    , H.text "Ensuite, à chaque tour, l'attaquant choisit d'attaquer un sommet puis le défenseur doit déplacer un de ses gardes"
    , H.text " vers le sommet attaqué à la condition que celui soit adjacent au garde."
    , H.br
    , H.text "Si le défenseur ne peut pas déplacer de garde, il perd la partie."
    , H.br
    , H.text "La partie peut ne pas avoir de fin. Le but est de déterminer le nombre minimum de gardes pour défendre infiniment toute attaque."
    , H.br
    , H.text "Dans une variante, le défenseur peut déplacer plusieurs gardes à chaque tour."
    ]

  winTitle = "L'attaquant gagne"