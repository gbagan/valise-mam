module UI.GraphEditor where

import MamPrelude
import Game.Helpers (pointerDecoder)
import Lib.Graph (Graph, Edge(..), Position)
import Lib.Graph as Graph
import Lib.Update (UpdateMam)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import UI.Dialog (dialog)
import UI.Icon (iconbutton, Icon(..))
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent as ME
import Web.PointerEvent.PointerEvent as PE

data Mode = VertexMode | AddEMode | DeleteMode | MoveMode

derive instance Eq Mode

emptyGraph ∷ Graph
emptyGraph = { vertices: [], edges: [], title: "Graphe personnalisé" }

type Model =
  { graph ∷ Graph
  , mode ∷ Mode
  , selectedVertex ∷ Maybe Int
  , currentPosition ∷ Maybe Position
  }

init ∷ Model
init = { graph: emptyGraph, mode: VertexMode, selectedVertex: Nothing, currentPosition: Nothing }

data Msg
  = AddVertex { x ∷ Number, y ∷ Number }
  | SelectVertex Int
  | PointerUp Int
  | Move { x ∷ Number, y ∷ Number }
  | DeleteVertex Int
  | DeleteEdge Edge
  | DropOrLeave
  | SetMode Mode
  | Clear

class MsgWithGEditor msg where
  geditormsg ∷ Msg → msg

update ∷ ∀ model msg. Lens' model Model → Msg → UpdateMam model msg Unit
update lens = case _ of
  AddVertex pos → do
    lens %= \model →
      if model.mode == VertexMode then
        model { graph = Graph.addVertex pos model.graph }
      else
        model

  SelectVertex i → do
    lens %= \model →
      if model.mode `elem` [ AddEMode, VertexMode ] then
        model { selectedVertex = Just i }
      else
        model

  DropOrLeave →
    lens %= \model →
      if model.mode == AddEMode then
        model { selectedVertex = Nothing }
      else
        model

  PointerUp i → do
    lens %= \model →
      case model.mode, model.selectedVertex of
        VertexMode, _ →
          model { selectedVertex = Nothing, currentPosition = Nothing }
        AddEMode, Just j →
          model { graph = Graph.addEdge i j model.graph, selectedVertex = Nothing }
        _, _ → model

  Move pos →
    lens %= \model →
      case model.mode, model.selectedVertex of
        VertexMode, Just i →
          model { graph = Graph.moveVertex i pos model.graph }
        AddEMode, _ →
          model { currentPosition = Just pos }
        _, _ → model

  DeleteVertex i → do
    lens %= \model →
      model { graph = Graph.removeVertex i model.graph }

  DeleteEdge (Edge u v) →
    lens %= \model →
      if model.mode == DeleteMode then
        model { graph = Graph.removeEdge u v model.graph }
      else
        model

  SetMode mode → lens %= \model → model { mode = mode }
  Clear → lens %= \model → model { graph = emptyGraph }

currentLine ∷ ∀ a. Position → Position → Html a
currentLine p1 p2 =
  S.line
    [ SA.x1 $ 100.0 * p1.x
    , SA.y1 $ 100.0 * p1.y
    , SA.x2 $ 100.0 * p2.x
    , SA.y2 $ 100.0 * p2.y
    , H.class_ "dessin-line-to-pointer"
    ]

view :: ∀ msg. MsgWithGEditor msg => Model → (Graph → msg) → Html msg
view { graph, mode, currentPosition, selectedVertex } onOk =
  dialog { title: "Crée ton graphe", onOk: Just (onOk graph), onCancel: Nothing }
    [ H.div [ H.class_ "flex ui-grapheditor" ]
        [ H.div [ H.class_ "ui-grapheditor-board" ]
            [ S.svg
                [ H.class_ "dessin-svg"
                , SA.viewBox 0.0 0.0 100.0 100.0
                , E.onClick' $ pointerDecoder (geditormsg <<< AddVertex)
                , E.onPointerUp \_ → geditormsg DropOrLeave
                , E.onPointerLeave \_ → geditormsg DropOrLeave
                , E.onPointerMove' $ pointerDecoder (geditormsg <<< AddVertex) <<< PE.toMouseEvent
                ]
                [ S.g []
                    $ graph.edges
                    <#> \edge →
                      H.maybe (Graph.getCoordsOfEdge graph edge) \{ px1, px2, py1, py2 } →
                        S.line
                          [ SA.x1 $ 100.0 * px1
                          , SA.y1 $ 100.0 * py1
                          , SA.x2 $ 100.0 * px2
                          , SA.y2 $ 100.0 * py2
                          , H.class_ "ui-grapheditor-edge"
                          , H.class' "deletemode" $ mode == DeleteMode
                          , E.onClick \_ → geditormsg (DeleteEdge edge)
                          ]
                , S.g []
                    $ graph.vertices
                    # mapWithIndex \i { x, y } →
                        S.circle
                          [ SA.cx $ 100.0 * x
                          , SA.cy $ 100.0 * y
                          , SA.r 3.0
                          , H.class_ "ui-grapheditor-vertex"
                          , H.class' "deletemode" $ mode == DeleteMode
                          , SA.stroke $ if selectedVertex == Just i then "red" else "blue"
                          --,   P.fill "blue"
                          , E.onClick' \ev →
                              if mode == VertexMode then do
                                stopPropagation $ ME.toEvent ev
                                pure Nothing
                              else if mode == DeleteMode then
                                pure $ Just $ geditormsg (DeleteVertex i)
                              else
                                pure Nothing
                          , E.onPointerDown' \ev → do
                              stopPropagation $ PE.toEvent ev
                              pure $ Just $ geditormsg (SelectVertex i)
                          , E.onPointerUp \_ → geditormsg (PointerUp i)
                          ]
                , H.when (mode == AddEMode) \_ →
                    H.fromMaybe case selectedVertex of
                      Just v → currentLine <$> currentPosition <*> (Graph.getCoords graph v)
                      _ → Nothing
                ]
            ]
        , H.div [ H.class_ "flex ui-grapheditor-buttons" ]
            [ iconbutton { icon: IconSymbol "#vertex", selected: mode == VertexMode, tooltip: Just "Ajoute ou déplace un sommet" }
                [ E.onClick \_ → geditormsg $ SetMode VertexMode ]
            , iconbutton { icon: IconSymbol "#edge", selected: mode == AddEMode, tooltip: Just "Connecte deux sommets" }
                [ E.onClick \_ → geditormsg $ SetMode AddEMode ]
            , iconbutton { icon: IconSymbol "#trash", selected: mode == DeleteMode, tooltip: Just "Enlève un sommet ou une arête" }
                [ E.onClick \_ → geditormsg $ SetMode DeleteMode ]
            , iconbutton { icon: IconSymbol "#clear", tooltip: Just "Efface tout le graphe" } [ E.onClick \_ → geditormsg Clear ]
            ]
        ]
    ]