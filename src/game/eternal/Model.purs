module Game.Eternal.Model where

import MyPrelude

import Data.Array.NonEmpty (elemLastIndex)
import Data.Lens (to)
import Effect.Exception (throwException)
import Game.Core (class Game, class MsgWithCore, CoreMsg, GState, playA, coreUpdate, _ext, genState, newGame, _position, defaultSizeLimit)
import Game.Effs (EFFS)
import Lib.Util (repeat)
import Pha.Update (Update, purely)

data Edge = Edge Int Int
infix 3 Edge as ↔
instance eqEdge ∷ Eq Edge where
    eq (u1 ↔ v1) (u2 ↔ v2) = u1 == u2  && v1 == v2 || u1 == v2 && u2 == v1
type Position = { x ∷ Number, y ∷ Number }

-- | une structure Graph est composé d'une liste des arêtes et de la position de chaque sommet dans le plan
type Graph = {vertices ∷ Array Position, edges ∷ Array Edge }


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
    ,   guards ∷ Array Int
    ,   multimove ∷ Array {from ∷ Int, to ∷ Int}
    }

newtype ExtState = Ext Ext'

type State = GState (Array (Maybe Int)) ExtState

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_graph ∷ Lens' State Graph
_graph = _ext' ∘ lens _.graph _{graph = _}
_guards ∷ Lens' State (Array Int)
_guards = _ext' ∘ lens _.guards _{guards = _}
_multimove ∷ Lens' State (Array {from ∷ Int, to ∷ Int})
_multimove = _ext' ∘ lens _.multimove _{multimove = _}

-- | état initial
istate ∷ State
istate = genState [] identity (Ext { graph: path(6), guards: [2, 3], multimove: []})

instance game ∷ Game (Array (Maybe Int)) ExtState (Maybe Int) where
    play state x = Nothing
    initialPosition _ = pure []
    onNewGame state = pure state
    isLevelFinished state = false
    computerMove _ = pure Nothing
    sizeLimit = defaultSizeLimit
    updateScore st = st ∧ true

data Msg = Core CoreMsg | AddToMultimove Int Int
instance withcore ∷ MsgWithCore Msg where core = Core
    
update ∷ Msg → Update State EFFS
update (Core msg) = coreUpdate msg
update (AddToMultimove from to) = purely $ over _multimove \moves →
    if from == to then
        moves # filter \{from: f} → f /= from
    else
        moves `snoc` {from, to}