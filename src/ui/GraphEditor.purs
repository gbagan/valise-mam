module UI.GraphEditor where

import MyPrelude

import Effect.Class (liftEffect)
import Game.Common (pointerDecoder)
import Lib.Graph (Graph, Position)
import Lib.Graph as Graph
import Lib.Update (Update, get)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import UI.Dialog (dialog)
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Mode = VertexMode | AddEMode | DeleteMode | MoveMode
derive instance Eq Mode

emptyGraph ∷ Graph
emptyGraph = { vertices: [], edges: [], title: "Graphe personnalisé" }

type Model =
    {   graph ∷ Graph
    ,   mode ∷ Mode
    ,   selectedVertex ∷ Maybe Int
    ,   currentPosition ∷ Maybe Position
    }

init ∷ Model
init = { graph: emptyGraph, mode: VertexMode, selectedVertex: Nothing, currentPosition: Nothing }

data Msg =
      AddVertex MouseEvent
    | SelectVertex Int MouseEvent
    | PointerUp Int MouseEvent
    | Move MouseEvent
    | DeleteVertex Int MouseEvent
    | DeleteEdge Int Int
    | SetMode Mode
    | Clear

class MsgWithGEditor msg where
    geditormsg ∷ Msg → msg


update ∷ forall st. Lens' st Model -> Msg -> Update st Unit
update lens = case _ of
    AddVertex ev -> do 
        pos <- liftEffect $ pointerDecoder ev
        case pos of
            Nothing -> pure unit
            Just p ->
                lens %= \model -> 
                    if model.mode == VertexMode then
                        model{graph = Graph.addVertex p model.graph}
                    else
                        model

    SelectVertex i ev -> do
        liftEffect $ stopPropagation $ toEvent ev
        lens %= \model ->
            if model.mode `elem` [AddEMode, VertexMode] then
                model{selectedVertex = Just i}
            else
                model

    PointerUp i ev -> do
        lens %= \model ->
            case model.mode /\ model.selectedVertex of
                VertexMode /\ _ ->
                    model{selectedVertex = Nothing, currentPosition = Nothing}
                AddEMode /\ Just j ->
                    model{graph = Graph.addEdge i j model.graph, selectedVertex = Nothing}
                _ -> model

    Move ev -> do 
        pos <- liftEffect $ pointerDecoder ev
        lens %= \model ->
            case pos /\ model.mode /\ model.selectedVertex of
                Just p /\ VertexMode /\ Just i ->
                    model{graph = Graph.moveVertex i p model.graph}
                Just p /\ AddEMode /\ _ ->
                    model{currentPosition = Just p}
                _ -> model

    DeleteVertex i ev -> do
        st <- get
        when ((st^.lens).mode == VertexMode)
            (liftEffect $ stopPropagation $ toEvent ev)
        lens %= \model ->
            if model.mode == DeleteMode then
                model{graph = Graph.removeVertex i model.graph}
            else
                model

    SetMode mode -> lens %= \model -> model{mode = mode}
    Clear -> lens %= \model -> model{graph = emptyGraph}
    _ -> pure unit

currentLine ∷ ∀a. Position → Position → Html a
currentLine p1 p2 =
    H.line
    [   P.x1 $ 100.0 * p1.x
    ,   P.y1 $ 100.0 * p1.y
    ,   P.x2 $ 100.0 * p2.x
    ,   P.y2 $ 100.0 * p2.y
    ,   H.class_ "dessin-line-to-pointer"
    ]

view :: forall msg. MsgWithGEditor msg => Model -> Html msg
view {graph, mode, currentPosition, selectedVertex} =
    dialog { title: "Crée ton graphe", onOk: Nothing, onCancel: Nothing }
    [   H.div [H.class_ "flex queens-custompiece"]
        [   H.div [H.class_ "queens-grid queens-custompiece-grid"]
            [   H.svg [ H.class_ "dessin-svg"
                      , P.viewBox 0 0 100 100
                      , E.onClick $ geditormsg <<< AddVertex
                      , E.onPointerMove $ geditormsg <<< Move
                      ] $ concat 
                [   graph.edges <#> \edge →
                    H.maybe (Graph.getCoordsOfEdge graph edge) \{px1, px2, py1, py2} →
                        H.line 
                        [   P.x1 $ 100.0 * px1
                        ,   P.y1 $ 100.0 * py1
                        ,   P.x2 $ 100.0 * px2
                        ,   P.y2 $ 100.0 * py2
                        ,   H.class_ "dessin-line2"
                        ]
                ,   graph.vertices # mapWithIndex \i {x, y} →
                            H.circle
                            [   P.cx $ 100.0 * x
                            ,   P.cy $ 100.0 * y
                            ,   P.r 3.0
                            ,   P.stroke $ if selectedVertex == Just i then "red" else "blue"
                            ,   P.fill "blue"
                            ,   E.onClick \ev -> geditormsg (DeleteVertex i ev)
                            ,   E.onPointerDown \ev -> geditormsg (SelectVertex i ev)
                            ,   E.onPointerUp \ev -> geditormsg (PointerUp i ev)
                            ]
                ,   [H.when (mode == AddEMode) \_ →
                        H.fromMaybe case selectedVertex of
                            Just v → currentLine <$> currentPosition <*> (Graph.getCoords graph v)
                            _ → Nothing
                    ]
                ]
            ]
        ,   H.div [H.class_ "flex  queens-custompiece-directions"] 
            [   H.button [E.onClick \_ -> geditormsg $ SetMode VertexMode] [H.text "Ajouter/déplacer un sommet"]
            ,   H.button [E.onClick \_ -> geditormsg $ SetMode DeleteMode] [H.text "Retirer un sommet/arête"]
            ,   H.button [E.onClick \_ -> geditormsg $ SetMode AddEMode] [H.text "Connecter deux sommets"]
            ,   H.button [E.onClick \_ -> geditormsg $ Clear] [H.text "Effacer tout"]
            ]
        ]
    ]