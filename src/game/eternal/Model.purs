module Game.Eternal.Model where
import MyPrelude
import Lib.Util (repeat)
import Pha.Update (Update)
import Game.Core (class Game, class MsgWithCore, CoreMsg, GState,
                  playA, coreUpdate, _ext, genState, newGame, _position, defaultSizeLimit)
import Game.Effs (EFFS)

data Edge = Edge Int Int
infix 3 Edge as ↔
instance eqEdge ∷ Eq Edge where
    eq (u1 ↔ v1) (u2 ↔ v2) = u1 == u2  && v1 == v2 || u1 == v2 && u2 == v1
type Position = { x ∷ Number, y ∷ Number }

-- | une structure Graph est composé d'une liste des arêtes et de la position de chaque sommet dans le plan
type Graph = {vertices ∷ Array Position, edges ∷ Array Edge }


path ∷ Int → Graph
path n =
    {   vertices: repeat n \i →  {x: toNumber i, y: toNumber i}
    ,   edges: repeat (n - 1) \i → i ↔ (i + 1)
    }

--circle ∷ Int → Graph
--circle n = g { edges = g.edges `snoc` 0 ↔ n }
--    where g = path n
{- 
graphs ∷ Array Graph
graphs = [house, ex1, ex2, ex3, cross]

type Ext' = {
    graphIndex ∷ Int,
    graph ∷ Graph
}
newtype ExtState = Ext Ext'

-- | une position est un chemin dans le graphe avec potentiellement des levés de crayon
-- | Just Int → un sommet
-- | Un levé de crayon

type State = GState (Array (Maybe Int)) ExtState

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_graphIndex ∷ Lens' State Int
_graphIndex = _ext' ∘ lens _.graphIndex _{graphIndex = _}
_graph ∷ Lens' State Graph
_graph = _ext' ∘ lens _.graph _{graph = _}

-- | état initial
istate ∷ State
istate = genState [] identity (Ext { graphIndex: 0, graph: house})

-- | l'ensemble des arêtes compososant un chemin contenant potentiellement des levés de crayon
edgesOf ∷ Array (Maybe Int) → Array Edge
edgesOf = mapMaybe toEdge ∘ pairwise where
    toEdge (Just u ∧ Just v) = Just (u ↔ v)
    toEdge _ = Nothing

instance game ∷ Game (Array (Maybe Int)) ExtState (Maybe Int) where
    play state x = 
        let position = state^._position in 
        case x ∧ last position of
            Nothing ∧ Just (Just _) → Just (position `snoc` x)
            Nothing ∧ _ → Nothing
            Just u ∧ Just (Just v) →
                    if not (elem (u↔v) (edgesOf position)) && elem (u↔v) (state^._graph).edges then
                        Just (position `snoc` x)
                    else
                        Nothing
            _ → Just (position `snoc` x)

    initialPosition _ = pure []
    onNewGame state = pure $ state # _graph .~ (graphs !! (state^._graphIndex) # fromMaybe house)
    isLevelFinished state = length (edgesOf (state^._position)) == length (state^._graph).edges
    computerMove _ = pure Nothing
    sizeLimit = defaultSizeLimit
    updateScore st = st ∧ true

-- | nombre de levés de crayon déjà effectués
nbRaises ∷ State → Int
nbRaises = view _position >>> filter isNothing >>> length

data Msg = Core CoreMsg | SetGraphIndex Int | Play (Maybe Int)
instance withcore ∷ MsgWithCore Msg where core = Core
    
update ∷ Msg → Update State EFFS
update (Core msg) = coreUpdate msg
update (SetGraphIndex i) = newGame (_graphIndex .~ i)
update (Play m) = playA m
