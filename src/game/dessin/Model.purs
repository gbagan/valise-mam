module Game.Dessin.Model where

import MamPrelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Game.Core (class Game, class ScoreGame, class MsgWithCore, CoreMsg, GState, Objective(..), ShowWinPolicy(..), Dialog(..),
                 updateScore', playA, coreUpdate, _ext, genState, newGame, _position, _dialog,
                 defaultSizeLimit, loadFromJson', saveToJson')
import Lib.Graph (Graph, Edge, (↔))
import Lib.Update (UpdateMam)
import Lib.Util (pairwise)
import UI.GraphEditor as GEditor

data Move = MoveTo Int | Raise
derive instance Eq Move

data GraphIndex = GraphIndex Int | CustomGraph
derive instance Eq GraphIndex

instance DecodeJson Move where
    decodeJson json = decodeJson json <#> case _ of
        Nothing → Raise
        Just y → MoveTo y

instance EncodeJson Move where
    encodeJson move = encodeJson case move of
        Raise → Nothing
        MoveTo x → Just x

house ∷ Graph
house =
    {   title: "Maison"
    ,   vertices: [{x: 1.0, y: 4.0}, {x: 3.0, y: 4.0 }, {x: 2.0, y: 3.0}, {x: 1.0, y: 2.0}, {x: 3.0, y: 2.0}, {x: 2.0, y: 1.0}]
    ,   edges: [0↔1, 1↔4, 4↔3, 3↔0, 2↔0, 2↔1, 2↔3, 2↔4, 3↔5, 4↔5]
    }

sablier ∷ Graph
sablier =
    {   title: "Sablier"
    ,   vertices: [{x: 1.0, y: 0.7}, {x: 4.0, y: 0.7}, {x: 2.5, y: 1.2}, {x: 2.5, y: 1.9},
                    {x: 1.5, y: 1.9}, {x: 3.5, y: 1.9}, {x: 1.5, y: 2.5}, {x: 3.5, y: 2.5},
                    {x: 2.5, y: 2.5}, {x: 2.5, y: 3.2}, {x: 1.0, y: 3.7}, {x: 4.0, y: 3.7}]
    ,   edges: [0↔1, 0↔2, 1↔2, 2↔3, 3↔4, 3↔5, 4↔6, 5↔7, 6↔8, 7↔8, 8↔9, 9↔10, 9↔11, 10↔11]
    }

house2 ∷ Graph
house2 =
    {   title: "Maison avec cave"
    ,   vertices: [{x: 2.0, y: 2.5}, {x: 1.0, y: 3.2}, {x: 3.0, y: 3.2 }, {x: 1.0, y: 1.8}, {x: 3.0, y: 1.8}, 
                    {x: 2.0, y: 1.0}, {x: 2.0, y: 4.0}]
    ,   edges: [0↔1, 0↔2, 0↔3, 0↔4, 1↔2, 2↔4, 4↔3, 3↔1, 3↔5, 4↔5, 1↔6, 2↔6]
    }

interlace ∷ Graph
interlace =
    {   title: "Entrelacements"
    ,   vertices: [ {x: 2.5, y: 1.0}, {x: 4.0, y: 1.0 },
                    {x: 1.0, y: 2.0}, {x: 2.5, y: 2.0}, {x: 3.25, y: 2.0},
                    {x: 2.5, y: 2.5}, {x: 3.25, y: 2.5}, {x: 4.0, y: 2.5},
                    {x: 1.0, y: 4.0}, {x: 3.25, y: 4.0}]
    ,   edges: [0↔1, 0↔3, 2↔3, 3↔4, 1↔7, 5↔6, 6↔7, 3↔5, 4↔6, 2↔8, 6↔9, 8↔9]
    }

grid ∷ Graph
grid = 
    {   title: "Grille"
    ,   vertices: [ {x: 0.0, y: 0.5}, {x: 2.0, y: 0.5}, {x: 4.0, y: 0.5},
                    {x: 1.0, y: 1.5}, {x: 3.0, y: 1.5 }, 
                    {x: 0.0, y: 2.5}, {x: 2.0, y: 2.5}, {x: 4.0, y: 2.5},
                    {x: 1.0, y: 3.5}, {x: 3.0, y: 3.5 },
                    {x: 0.0, y: 4.5}, {x: 2.0, y: 4.5}, {x: 4.0, y: 4.5}
                    ] <#> \{x, y} → {x: x*0.85+1.0, y: y*0.85+0.21}
    ,   edges: [0↔1, 1↔2, 0↔3, 1↔3, 1↔4, 2↔4, 3↔5, 3↔6, 4↔6, 4↔7,
                5↔8, 6↔8, 6↔9, 7↔9, 8↔10, 8↔11, 9↔11, 9↔12, 10↔11, 11↔12,
                0↔5, 2↔7, 5↔10, 7↔12]
    }

konisberg ∷ Graph
konisberg = 
    {   title: "Ponts de Königsberg"
    ,   vertices: [ {x: 1.0, y: 0.0}, {x: 3.0, y: 0.0 }, {x: 0.0, y: 1.0}, {x: 2.0, y: 1.0}, {x: 4.0, y: 1.0},
                    {x: 1.0, y: 2.0}, {x: 3.0, y: 2.0}, {x: 0.0, y: 3.0}, {x: 2.0, y: 4.0}, {x: 4.0, y: 3.0}
                    ] <#> \{x, y} → {x: x*0.85+1.0, y: y*0.85+1.0}
    ,   edges: [2↔0, 0↔3, 3↔1, 1↔4, 4↔6, 6↔3, 3↔5, 5↔2, 2↔7, 3↔8, 4↔9, 7↔8, 8↔9]  
    }

ex1 ∷ Graph
ex1 = 
    {   title: "Tour"
    ,   vertices: [ {x: 1.0, y: 0.0}, {x: 0.0, y: 1.0 }, {x: 2.0, y: 1.0}, {x: 1.0, y: 2.0},
                    {x: 0.0, y: 3.0 }, {x: 2.0, y: 3.0}, {x: 1.0, y: 4.0}, {x: 0.0, y: 5.0}, {x: 2.0, y: 5.0}
                  ] <#> \{x, y} → {x: x*0.9+1.0, y: y*0.9+0.2}
    ,   edges: [0↔1, 0↔2, 1↔2, 1↔3, 2↔3, 3↔4, 3↔5, 4↔5, 1↔4, 2↔5, 4↔6, 5↔6, 6↔7, 6↔8, 4↔7, 5↔8]  
}

ex3 ∷ Graph
ex3 =
    {   title: "Soleil"
    ,   vertices: [ {x: 1.0, y: 2.0}, {x: 2.0, y: 1.0 }, {x: 3.0, y: 2.0}, {x: 2.0, y: 3.0},
                    {x: 0.5, y: 0.5 }, {x: 0.5, y: 3.5}, {x: 3.5, y: 3.5}, {x: 3.5, y: 0.5}, {x: 2.0, y: 2.0}]
                    <#> \{x, y} → {x: x * 1.15, y: y * 1.15}
    ,   edges: [0↔1, 1↔2, 2↔3, 3↔0, 0↔8, 1↔8, 2↔8, 3↔8, 0↔4, 1↔4, 0↔5, 3↔5, 2↔6, 3↔6, 1↔7, 2↔7]
}

city ∷ Graph
city =
    {   title: "Ville folle"
    ,   vertices: [ {x: 0.0, y: 0.0}, {x: 2.0, y: 0.0}, {x: 3.0, y: 0.0},
                    {x: 0.0, y: 1.0}, {x: 1.0, y: 1.0}, {x: 2.0, y: 1.0}, {x: 3.0, y: 1.0},
                    {x: 1.0, y: 2.0}, {x: 2.0, y: 2.0}, {x: 3.0, y: 2.0}, {x: 4.0, y: 2.0},
                    {x: 1.0, y: 3.0}, {x: 2.0, y: 3.0}, {x: 4.0, y: 3.0},
                    {x: 0.0, y: 4.0}, {x: 1.0, y: 4.0}, {x: 3.0, y: 4.0}, {x: 4.0, y: 4.0},
                    {x: 1.0, y: 5.0}, {x: 2.0, y: 5.0}, {x: 3.0, y: 5.0}, {x: 4.0, y: 5.0}
                ] <#> \{x, y} → {x: x*0.75+1.0, y: y*0.72+0.6}
    , edges: [1↔2, 0↔3, 0↔4, 1↔4, 2↔5, 4↔5,
              3↔7, 4↔8, 7↔8, 5↔8, 5↔9, 6↔9, 6↔10,
              8↔11, 9↔12, 11↔12, 10↔13,
              14↔15, 15↔16, 16↔17, 9↔16, 12↔16, 13↔16,
              14↔18, 15↔19, 16↔20, 17↔21, 18↔19, 20↔21
            ]
    }


owl ∷ Graph
owl =
    {   title: "Hibou"
    ,   vertices: [ {x: 0.0, y: 0.0}, {x: 2.0, y: 0.0},
                    {x: 0.0, y: 1.0}, {x: 1.0, y: 1.0}, {x: 2.0, y: 1.0}, {x: 3.0, y: 1.0},
                    {x: 0.0, y: 2.0}, {x: 1.0, y: 2.0}, {x: 2.0, y: 2.0}, {x: 3.0, y: 2.0},
                    {x: 1.0, y: 3.0}, {x: 2.0, y: 3.0}, {x: 4.0, y: 3.0},
                    {x: 1.0, y: 4.0}, {x: 2.0, y: 4.0}, {x: 3.0, y: 4.0}, {x: 4.0, y: 4.0},
                    {x: 1.0, y: 5.0}, {x: 3.0, y: 5.0}
                ] <#> \{x, y} → {x: x*0.8+0.5, y: y*0.8+0.6}
    , edges: [0↔2, 0↔3, 1↔3, 1↔4, 2↔3, 3↔4, 4↔5,
              2↔6, 2↔7, 3↔7, 3↔8, 4↔8, 4↔9, 5↔9, 6↔7, 7↔8,
              7↔10, 7↔11, 8↔11, 9↔11, 9↔12, 
              10↔14, 11↔15, 12↔15, 9↔15, 13↔14, 14↔15, 15↔16,
              13↔17, 14↔17, 15↔18, 16↔18
            ]
    }

rabbit ∷ Graph
rabbit =
    {   title: "Lièvre bondissant"
    ,   vertices: [ {x: 1.0, y: 0.0}, {x: 2.0, y: 0.0}, {x: 4.0, y: 0.0},
                    {x: 2.0, y: 1.0}, {x: 3.0, y: 1.0}, {x: 4.0, y: 1.0}, 
                    {x: 1.0, y: 2.0}, {x: 2.0, y: 2.0}, {x: 3.0, y: 2.0}, {x: 4.0, y: 2.0},
                    {x: 0.0, y: 3.0}, {x: 2.0, y: 3.0}, {x: 3.0, y: 3.0}, {x: 4.0, y: 3.0},
                    {x: 0.0, y: 4.0}, {x: 1.0, y: 4.0}, {x: 2.0, y: 4.0}, {x: 3.0, y: 4.0},
                    {x: 1.0, y: 5.0}
                ] <#> \{x, y} → {x: x*0.8+1.0, y: y*0.8+0.8}
    , edges: [0↔1, 0↔3, 1↔4, 2↔4, 2↔5, 3↔4, 4↔5,
              3↔6, 3↔7, 4↔7, 4↔8, 5↔8, 5↔9, 7↔8, 8↔9,
              6↔10, 6↔11, 7↔11, 9↔12, 12↔13,
              6↔15, 10↔15, 11↔15, 14↔15, 15↔16, 12↔16, 12↔17, 13↔17,
              14↔18, 15↔18]
    }


cross ∷ Graph
cross = 
    {   title: "Croix"
    ,   vertices: [ {x: 0.0, y: 1.0}, {x: 0.0, y: 2.0}, {x: 0.5, y: 1.5}, -- 0 -- 2
                    {x: 1.0, y: 0.0}, {x: 1.0, y: 1.0}, {x: 1.0, y: 2.0}, {x: 1.0, y: 3.0}, -- 3 -- 6
                    {x: 1.5, y: 0.5}, {x: 1.5, y: 1.5}, {x: 1.5, y: 2.5}, -- 7 -- 9
                    {x: 2.0, y: 0.0}, {x: 2.0, y: 1.0}, {x: 2.0, y: 2.0}, {x: 2.0, y: 3.0}, -- 10 -- 13
                    {x: 2.5, y: 1.5}, {x: 3.0, y: 1.0}, {x: 3.0, y: 2.0} -- 14 -- 16
                    ] <#> \{x, y} → {x: x * 1.3 + 0.5, y: y * 1.3 + 0.5}
    ,   edges: [0↔1, 0↔2, 1↔2, 0↔4, 1↔5, 2↔4, 2↔5,
                3↔4, 4↔5, 5↔6, 3↔7, 4↔7, 4↔8, 5↔8, 5↔9, 6↔9,
                3↔10, 4↔11, 5↔12, 6↔13, 7↔10, 7↔11, 8↔11, 8↔12, 9↔12, 9↔13,
                10↔11, 11↔12, 12↔13, 11↔14, 12↔14, 11↔15, 12↔16, 14↔15, 14↔16, 15↔16]
    }

graphs ∷ Array Graph
graphs = [house, house2, sablier, interlace, grid, konisberg, ex1, ex3, city, owl, rabbit, cross] 
            <#> \g → g{ vertices = g.vertices <#> \{x, y} → {x: x / 5.0, y : y / 5.0}  }

nbGraphs ∷ Int
nbGraphs = length graphs

type Ext' =
    {   graphIndex ∷ GraphIndex
    ,   graph ∷ Graph
    ,   graphEditor ∷ GEditor.Model
    }
newtype ExtState = Ext Ext'

-- | une position est un chemin dans le graphe avec potentiellement des levés de crayon
-- | Just Int → un sommet
-- | Un levé de crayon

type State = GState (Array Move) ExtState

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_graphIndex ∷ Lens' State GraphIndex
_graphIndex = _ext' ∘ prop (Proxy ∷ _ "graphIndex")
_graph ∷ Lens' State Graph
_graph = _ext' ∘ prop (Proxy ∷ _ "graph")
_graphEditor ∷ Lens' State GEditor.Model
_graphEditor = _ext' ∘ prop (Proxy ∷ _ "graphEditor")

-- | état initial
istate ∷ State
istate = genState [] identity (Ext { graphIndex: GraphIndex 0, graph: house, graphEditor: GEditor.init})

-- | l'ensemble des arêtes compososant un chemin contenant potentiellement des levés de crayon
edgesOf ∷ Array Move → Array Edge
edgesOf = mapMaybe toEdge ∘ pairwise where
    toEdge (MoveTo u ∧ MoveTo v) = Just (u ↔ v)
    toEdge _ = Nothing

instance Game (Array Move) ExtState Move where
    name _ = "dessin"

    play state x =
        let position = state^._position in 
        case x, last position of
            Raise, Just (MoveTo _) → Just (position `snoc` x)
            Raise, _ → Nothing
            MoveTo u, Just (MoveTo v) →
                    if not (elem (u↔v) (edgesOf position)) && elem (u↔v) (state^._graph).edges then
                        Just (position `snoc` x)
                    else
                        Nothing
            _, _ → Just (position `snoc` x)

    initialPosition _ = pure []
    onNewGame state = 
        let graph = case state^._graphIndex of
                        GraphIndex i → graphs !! i ?: house
                        CustomGraph → state^._graph
        in
        pure $ state # _graph .~ graph

    isLevelFinished state = length (edgesOf (state^._position)) == length (state^._graph).edges
    updateScore = updateScore' {onlyWhenFinished: true, showWin: ShowWinOnNewRecord}

    saveToJson = saveToJson'
    loadFromJson = loadFromJson'

    computerMove _ = pure Nothing
    sizeLimit = defaultSizeLimit
    onPositionChange = identity

instance ScoreGame (Array Move) ExtState Move where
    objective _ = Minimize
    scoreFn = nbRaises
    scoreHash state = case state^._graphIndex of
        GraphIndex i → show i
        CustomGraph → "custom"
    isCustomGame state = case state^._graphIndex of
        CustomGraph → true
        _ → false

-- | nombre de levés de crayon déjà effectués
nbRaises ∷ State → Int
nbRaises = length ∘ filter (_ == Raise) ∘ view _position

data Msg = Core CoreMsg | GEditor GEditor.Msg | SetGraphIndex GraphIndex | Play Move | CloseEditor Graph
instance MsgWithCore Msg where core = Core
instance GEditor.MsgWithGEditor Msg where geditormsg = GEditor
    
update ∷ Msg → UpdateMam State
update (Core msg) = coreUpdate msg
update (GEditor msg) = GEditor.update _graphEditor msg
update (SetGraphIndex i) = case i of
    GraphIndex _ → newGame $ _graphIndex .~ i
    CustomGraph →  newGame \st →
                        st # _dialog .~ CustomDialog
                           # _graphIndex .~ CustomGraph
update (Play m) = playA m
update (CloseEditor g) = modify_ $ (_dialog .~ NoDialog) >>> (_graph .~ g)