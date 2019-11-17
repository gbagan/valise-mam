module Game.Dessin.Model where
import MyPrelude
import Lib.Util (pairwise)
import Pha.Action (Action, RNG)
import Game.Core (class Game, GState(..), genState, newGame', _position, defaultSizeLimit)

data Edge = Edge Int Int
infix 3 Edge as ↔
instance eqEdge :: Eq Edge where
    eq (u1 ↔ v1) (u2 ↔ v2) = u1 == u2  && v1 == v2 || u1 == v2 && u2 == v1
type Position = { x :: Number, y :: Number }

-- une structure Graph est composé d'une liste des arêtes et de la position de chaque sommet dans le plan
type Graph = {vertices :: Array Position, edges :: Array Edge }


house :: Graph
house = {
    vertices: [{x: 1.0, y: 4.0}, {x: 3.0, y: 4.0 }, {x: 1.0, y: 2.0}, {x: 3.0, y: 2.0}, {x: 2.0, y: 1.0}],
    edges: [0↔1, 0↔2, 0↔3, 1↔2, 1↔3, 2↔3, 2↔4, 3↔4]
}

ex1 :: Graph
ex1 = {
    vertices: [{x: 1.0, y: 0.0}, {x: 0.0, y: 1.0 }, {x: 2.0, y: 1.0}, {x: 1.0, y: 2.0},
               {x: 0.0, y: 3.0 }, {x: 2.0, y: 3.0}, {x: 1.0, y: 4.0}, {x: 0.0, y: 5.0}, {x: 2.0, y: 5.0}
                ]
               <#> \{x, y} -> {x: x*0.9+1.0, y: y*0.9+0.2},
    edges: [0↔1, 0↔2, 1↔2, 1↔3, 2↔3, 3↔4, 3↔5, 4↔5, 1↔4, 2↔5, 4↔6, 5↔6, 6↔7, 6↔8, 4↔7, 5↔8]  
}

ex2 :: Graph
ex2 = {
    vertices: [{x: 1.0, y: 2.0}, {x: 2.0, y: 1.0 }, {x: 3.0, y: 2.0}, {x: 2.0, y: 3.0},
               {x: 1.0, y: 1.0 }, {x: 1.0, y: 3.0}, {x: 3.0, y: 3.0}, {x: 3.0, y: 1.0}]
               <#> \{x, y} -> {x: x * 1.3, y: y * 1.3 - 0.3},
    edges: [0↔1, 1↔2, 2↔3, 3↔0, 0↔2, 0↔4, 1↔4, 0↔5, 3↔5, 2↔6, 3↔6, 1↔7, 2↔7]  
}

ex3 :: Graph
ex3 = {
    vertices: [{x: 1.0, y: 2.0}, {x: 2.0, y: 1.0 }, {x: 3.0, y: 2.0}, {x: 2.0, y: 3.0},
               {x: 1.0, y: 1.0 }, {x: 1.0, y: 3.0}, {x: 3.0, y: 3.0}, {x: 3.0, y: 1.0}, {x: 2.0, y: 2.0}]
               <#> \{x, y} -> {x: x * 1.3, y: y * 1.3 - 0.3},
    edges: [0↔1, 1↔2, 2↔3, 3↔0, 0↔8, 1↔8, 2↔8, 3↔8, 0↔4, 1↔4, 0↔5, 3↔5, 2↔6, 3↔6, 1↔7, 2↔7]  
}

cross :: Graph
cross = {
    vertices: [{x: 0.0, y: 1.0}, {x: 0.0, y: 2.0}, {x: 0.5, y: 1.5}, -- 0 -- 2
               {x: 1.0, y: 0.0}, {x: 1.0, y: 1.0}, {x: 1.0, y: 2.0}, {x: 1.0, y: 3.0}, -- 3 -- 6
               {x: 1.5, y: 0.5}, {x: 1.5, y: 1.5}, {x: 1.5, y: 2.5}, -- 7 -- 9
               {x: 2.0, y: 0.0}, {x: 2.0, y: 1.0}, {x: 2.0, y: 2.0}, {x: 2.0, y: 3.0}, -- 10 -- 13
               {x: 2.5, y: 1.5}, {x: 3.0, y: 1.0}, {x: 3.0, y: 2.0} -- 14 -- 16
        ] <#> \{x, y} -> {x: x * 1.3 + 0.5, y: y * 1.3 + 0.5},
    edges: [0↔1, 0↔2, 1↔2, 0↔4, 1↔5, 2↔4, 2↔5,
        3↔4, 4↔5, 5↔6, 3↔7, 4↔7, 4↔8, 5↔8, 5↔9, 6↔9,
        3↔10, 4↔11, 5↔12, 6↔13, 7↔10, 7↔11, 8↔11, 8↔12, 9↔12, 9↔13,
        10↔11, 11↔12, 12↔13, 11↔14, 12↔14, 11↔15, 12↔16, 14↔15, 14↔16, 15↔16]
}

graphs :: Array Graph
graphs = [house, ex1, ex2, ex3, cross]

type Ext' = {
    graphIndex :: Int,
    graph :: Graph
}
newtype ExtState = Ext Ext'
type State = GState (Array (Maybe Int)) ExtState

-- lenses
_ext :: Lens' State Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_graphIndex :: Lens' State Int
_graphIndex = _ext ∘ lens _.graphIndex _{graphIndex = _}
_graph :: Lens' State Graph
_graph = _ext ∘ lens _.graph _{graph = _}

-- état initial
istate :: State
istate = genState [] identity (Ext { graphIndex: 0, graph: house})

-- l'ensemble des arêtes composoant un chemin contenant potentiellement des levés de crayon
edgesOf :: Array (Maybe Int) -> Array Edge
edgesOf = mapMaybe toEdge ∘ pairwise where
    toEdge (Just u ~ Just v) = Just (u ↔ v)
    toEdge _ = Nothing

instance gameDessin :: Game (Array (Maybe Int)) ExtState (Maybe Int) where
    play state v = (state^._position) `snoc` v
    canPlay state x = let position = state^._position in 
        case x of
            Nothing -> not (null position) && isJust (join (last position))
            Just v -> case last position of
                Just (Just u) -> not (elem (u↔v) (edgesOf position)) && elem (u↔v) (state^._graph).edges
                _ -> true

    initialPosition _ = pure []
    onNewGame state = pure $ state # _graph .~ (graphs !! (state^._graphIndex) # fromMaybe house)
    isLevelFinished state = length (edgesOf (state^._position)) == length (state^._graph).edges
    computerMove _ = Nothing
    sizeLimit = defaultSizeLimit
    updateScore st = st ~ true

-- nombre de levés de crayon déjà effectués
nbRaises :: State -> Int
nbRaises = length ∘ filter isNothing ∘ view _position

setGraphIndexA :: ∀effs. Int -> Action State (rng :: RNG | effs)
setGraphIndexA = newGame' (set _graphIndex)
