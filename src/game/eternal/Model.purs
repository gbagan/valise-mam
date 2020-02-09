module Game.Eternal.Model where

import MyPrelude

import Data.Array.NonEmpty (elemLastIndex)
import Data.Lens (to)
import Data.Newtype (overF)
import Effect.Exception (throwException)
import Game.Core (class Game, class MsgWithCore, CoreMsg, GState, playA, coreUpdate, _ext, genState, newGame, _position, defaultSizeLimit)
import Game.Effs (EFFS)
import Lib.Util (repeat)
import Pha.Update (Update, getState, purely)

data Edge = Edge Int Int
infix 3 Edge as ↔
instance eqEdge ∷ Eq Edge where
    eq (u1 ↔ v1) (u2 ↔ v2) = u1 == u2  && v1 == v2 || u1 == v2 && u2 == v1
type Pos = { x ∷ Number, y ∷ Number }

-- | une structure Graph est composée d'une liste des arêtes et de la position de chaque sommet dans le plan
type Graph = {vertices ∷ Array Pos, edges ∷ Array Edge }

-- | une position est composée de la position des gardes et éventuellement d'un sommet attaqué
type Position = {guards ∷ Array Int, attacked ∷ Maybe Int}

-- | un move est soit un sommet attaqué en cas d'attaque,
-- | soit un ensemble de déplacéments de gardes en cas de défense
type Multimove = Array {from ∷ Int, to ∷ Int}
data Move = Attack Int | Defense Multimove

path ∷ Int → Graph
path n =
    {   vertices: repeat n \i → 
            {   x: 0.50 + 0.35 * cos (toNumber i * 2.0 * pi / toNumber n)
            ,   y: 0.50 + 0.35 * sin (toNumber i * 2.0 * pi / toNumber n)
            }
    ,   edges: repeat (n - 1) \i → i ↔ (i + 1)
    }

circle ∷ Int → Graph
circle n = g { edges = g.edges `snoc` (0 ↔ n) }
    where g = path n
{- 
graphs ∷ Array Graph
graphs = [house, ex1, ex2, ex3, cross]
-}

type Ext' = 
    {   graph ∷ Graph
    ,   multimove ∷ Array {from ∷ Int, to ∷ Int}
    }

newtype ExtState = Ext Ext'

type State = GState Position ExtState

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_graph ∷ Lens' State Graph
_graph = _ext' ∘ lens _.graph _{graph = _}
_multimove ∷ Lens' State (Array {from ∷ Int, to ∷ Int})
_multimove = _ext' ∘ lens _.multimove _{multimove = _}

-- | état initial
istate ∷ State
istate = genState 
            {guards: [], attacked: Nothing}
            identity
            (Ext { graph: path(6), multimove: []})

moveGuards ∷ Array Int → Multimove → Array Int
moveGuards guards multimove =
    foldr
        (\{from, to} -> map \x -> if x == from then to else x)
        guards
        multimove

instance game ∷ Game {guards ∷ Array Int, attacked ∷ Maybe Int} ExtState Move where
    play state (Defense mmove) =
        case (state^._position).attacked of
            Just attacked ->
                let guards = moveGuards (state^._position).guards mmove  in
                if elem attacked guards then
                    Just {attacked: Nothing, guards}
                else
                    Nothing
            _ -> Nothing

    play state (Attack x) =
        if elem x (state^._position).guards || isJust (state^._position).attacked then
            Nothing
        else
            Just $ (state^._position) { attacked = Just x}

    initialPosition _ = pure {guards: [1, 3], attacked: Nothing}
    onNewGame state = pure state
    isLevelFinished state = false
    computerMove _ = pure Nothing
    sizeLimit = defaultSizeLimit
    updateScore st = st ∧ true

data Msg = Core CoreMsg | AddToMultimove Int Int | Play Int
instance withcore ∷ MsgWithCore Msg where core = Core
    
update ∷ Msg → Update State EFFS
update (Core msg) = coreUpdate msg
update (Play x) = do
    st <- getState
    case (st^._position).attacked of
        Just attacked -> playA (Defense [{from: x, to: attacked}])
        Nothing -> playA (Attack x)

update (AddToMultimove from to) = purely $ over _multimove \moves →
    if from == to then
        moves # filter \{from: f} → f /= from
    else
        moves `snoc` {from, to}