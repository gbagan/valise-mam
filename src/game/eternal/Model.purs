module Game.Eternal.Model where

import MyPrelude

import Game.Core (class Game, class MsgWithCore, CoreMsg, GState, SizeLimit(..), playA, coreUpdate, _ext, genState, newGame, _position, _nbRows)
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

data GraphKind = Path | Cycle
derive instance eqgkind ∷ Eq GraphKind

data Rules = OneGuard | ManyGuards
derive instance eqrules ∷ Eq Rules

-- | une position est composée de la position des gardes et éventuellement d'un sommet attaqué
type Position = {guards ∷ Array Int, attacked ∷ Maybe Int}

-- | un move est soit un sommet attaqué en cas d'attaque,
-- | soit un ensemble de déplacéments de gardes en cas de défense
type Multimove = Array {from ∷ Int, to ∷ Int}
data Move = Attack Int | Defense Multimove

data Phase = PrepPhase | GamePhase
derive instance eqPhase ∷ Eq Phase

path ∷ Int → Graph
path n =
    {   vertices: repeat n \i → 
            {   x: 0.50 + 0.35 * cos (toNumber i * 2.0 * pi / toNumber n)
            ,   y: 0.50 + 0.35 * sin (toNumber i * 2.0 * pi / toNumber n)
            }
    ,   edges: repeat (n - 1) \i → i ↔ (i + 1)
    }

cycle ∷ Int → Graph
cycle n = g { edges = g.edges `snoc` (0 ↔ (n-1)) }
    where g = path n
{- 
graphs ∷ Array Graph
graphs = [house, ex1, ex2, ex3, cross]
-}

type Ext' = 
    {   graph ∷ Graph
    ,   multimove ∷ Multimove
    ,   phase ∷ Phase
    ,   graphkind ∷ GraphKind
    ,   draggedGuard ∷ Maybe Int
    ,   rules ∷ Rules
    }

newtype ExtState = Ext Ext'

type State = GState Position ExtState

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_graph ∷ Lens' State Graph
_graph = _ext' ∘ lens _.graph _{graph = _}
_multimove ∷ Lens' State Multimove
_multimove = _ext' ∘ lens _.multimove _{multimove = _}
_phase ∷ Lens' State Phase
_phase = _ext' ∘ lens _.phase _{phase = _}
_graphkind ∷ Lens' State GraphKind
_graphkind = _ext' ∘ lens _.graphkind _{graphkind = _}
_rules ∷ Lens' State Rules
_rules = _ext' ∘ lens _.rules _{rules = _}
_draggedGuard ∷ Lens' State (Maybe Int)
_draggedGuard = _ext' ∘ lens _.draggedGuard _{draggedGuard = _}

_guards ∷ Lens' Position (Array Int)
_guards = lens _.guards _{guards = _}
_attacked ∷ Lens' Position (Maybe Int)
_attacked = lens _.attacked _{attacked = _}


-- | état initial
istate ∷ State
istate = genState 
            {guards: [], attacked: Nothing}
            _{nbRows = 6, customSize = true}
            (Ext { graphkind: Path, graph: path 1, multimove: [], phase: PrepPhase, draggedGuard: Nothing, rules: OneGuard})


isValidMultimove ∷ State → Multimove → Boolean
isValidMultimove st mmove =
    case (st^._position).attacked of
        Nothing → false
        Just attack →
            let edges = (st^._graph).edges 
                dests = mmove <#> _.to
            in
            elem attack dests && 
                (mmove # all \{from, to} -> elem (from ↔ to) edges)

moveGuards ∷ Array Int → Multimove → Array Int
moveGuards guards multimove = guards <#> \guard →
    case multimove # find \{from, to} → from == guard of
        Nothing → guard
        Just {from, to} → to

instance game ∷ Game {guards ∷ Array Int, attacked ∷ Maybe Int} ExtState Move where
    play state (Defense mmove) =
        case (state^._position).attacked of
            Just attacked ->
                if isValidMultimove state mmove then
                    let guards = moveGuards (state^._position).guards mmove
                    in
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
    
    onNewGame state =
        case state^._graphkind of
            Path -> pure (state # _graph .~ path (state^._nbRows))
            Cycle -> pure(state # _graph .~ cycle (state^._nbRows))

    isLevelFinished state =
        let guards = (state^._position).guards 
            edges = (state^._graph).edges
        in
        case (state^._position).attacked of
            Nothing → false
            Just attack →
                guards # all \guard → not (elem (guard ↔ attack) edges)


    computerMove _ = pure Nothing
    sizeLimit = const (SizeLimit 3 0 10 0)
    updateScore st = st ∧ true


toggleGuard ∷ Int → Array Int → Array Int
toggleGuard x l = if elem x l then filter (_ /= x) l else l `snoc` x

addToMultimove ∷ Array Edge → Int → Int → Multimove → Multimove
addToMultimove edges from to mmove =
    let mmove2 = mmove # filter \{from: from2} -> from /= from2 in
    if from == to || not (elem (from ↔ to) edges) then
        mmove2
    else
        mmove2 `snoc` {from, to}  

dragGuard ∷ Maybe Int → State → State
dragGuard to st =
    case st^._draggedGuard of
        Nothing -> st
        Just from ->
            let to2 = fromMaybe from to in
            st # _multimove %~ addToMultimove (st^._graph).edges from to2 # _draggedGuard .~ Nothing


data Msg = Core CoreMsg | SetGraphKind GraphKind | SetRules Rules | AddToMultimove Int Int 
            | DragGuard Int | DropGuard Int | LeaveGuard | DropOnBoard
            | StartGame | ToggleGuard Int | Play Int
instance withcore ∷ MsgWithCore Msg where core = Core
    
update ∷ Msg → Update State EFFS
update (Core msg) = coreUpdate msg
update (SetGraphKind kind) = newGame (_graphkind .~ kind)
update (SetRules rules) = newGame (_rules .~ rules)
update StartGame = purely $ _phase .~ GamePhase
update (ToggleGuard x) = pure unit
update (DragGuard x) = purely $ _draggedGuard .~ Just x
update (DropGuard to) = purely $ dragGuard (Just to)

update LeaveGuard = purely $ _draggedGuard .~ Nothing
update DropOnBoard = purely $ dragGuard Nothing

update (Play x) = do
    st <- getState
    case st^._phase /\ (st^._position).attacked  of
        PrepPhase /\ _ -> purely $ _position <<< _guards %~ toggleGuard x
        GamePhase /\ Just attacked -> playA (Defense [{from: x, to: attacked}])
        GamePhase /\ Nothing -> playA (Attack x)

update (AddToMultimove from to) = purely $ over _multimove \moves →
    if from == to then
        moves # filter \{from: f} → f /= from
    else
        moves `snoc` {from, to}