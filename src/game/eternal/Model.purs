module Game.Eternal.Model where

import MyPrelude
import Game.Core (class Game, class MsgWithCore, CoreMsg, GState, SizeLimit(..), Mode(..),
                    playA, coreUpdate, _ext, genState, newGame, isLevelFinished,
                    _position, _mode, _nbRows, _nbColumns)
import Game.Effs (EFFS)
import Pha.Random as R
import Lib.Util (repeat, repeat2, (..))
import Pha.Update (Update, get, modify)

-- type d'arête d'un graphe
data Edge = Edge Int Int
infix 3 Edge as ↔
instance eqEdge ∷ Eq Edge where
    eq (u1 ↔ v1) (u2 ↔ v2) = u1 == u2  && v1 == v2 || u1 == v2 && u2 == v1


type Pos = { x ∷ Number, y ∷ Number }

-- | une structure Graph est composée d'une liste des arêtes et de la position de chaque sommet dans le plan (entre 0 et 1)
type Graph = {vertices ∷ Array Pos, edges ∷ Array Edge }

data GraphKind = Path | Cycle | Grid | Biclique | Sun
derive instance eqgkind ∷ Eq GraphKind

-- règles du jeu: un seul garde peut se déplacer ou bien plusieurs à chaque tour
data Rules = OneGuard | ManyGuards
derive instance eqrules ∷ Eq Rules

-- | une position est composée de la position des gardes et éventuellement d'un sommet attaqué
type Position = {guards ∷ Array Int, attacked ∷ Maybe Int}

-- | un move est soit un sommet attaqué en cas d'attaque,
-- | soit un ensemble de déplacéments de gardes en cas de défense
data Move = Attack Int | Defense (Array Int)

-- | PrepPhase: phase de préparation (positionnement des gardes)
-- | GamePhase: phase où le défenseur et l'attaquant jouent

data Phase = PrepPhase | GamePhase
derive instance eqPhase ∷ Eq Phase

-- | generate a path graph
path ∷ Int → Graph
path n =
    {   vertices: repeat n \i → 
            {   x: 0.50 + 0.35 * cos (toNumber i * 2.0 * pi / toNumber n)
            ,   y: 0.50 + 0.35 * sin (toNumber i * 2.0 * pi / toNumber n)
            }
    ,   edges: repeat (n - 1) \i → i ↔ (i + 1)
    }

-- | generate a cycle graph
cycle ∷ Int → Graph
cycle n = g { edges = g.edges `snoc` (0 ↔ (n-1)) }
    where g = path n

-- | generate a grid graph
grid ∷ Int → Int → Graph
grid n m =
    let p = max n m in
    {   vertices: repeat2 n m \i j →
            {   x: 0.15 + 0.7 * toNumber i / toNumber (p-1)
            ,   y: 0.1 + 0.7 * toNumber j / toNumber (p-1)
            }
    ,   edges: (repeat2 n (m-1) \i j → (i*m+j) ↔ (i*m+j+1))
                <> (repeat2 (n-1) m \i j → (i*m+j) ↔ (i*m+j+m))
    }

-- | generate a star graph
star ∷ Int → Graph
star n =
    {   vertices: {x: 0.5, y: 0.5} `cons` repeat (n-1) \i → 
            {   x: 0.50 + 0.35 * cos (toNumber i * 2.0 * pi / toNumber (n-1))
            ,   y: 0.50 + 0.35 * sin (toNumber i * 2.0 * pi / toNumber (n-1))
            }
    ,   edges: repeat (n - 1) \i → (i + 1) ↔ 0
    }

-- | generate a sun graph
biclique ∷ Int → Int → Graph
biclique m n =
    {   vertices: repeat (n+m) \i → 
            {   x: if i < n then 0.2 else 0.8
            ,   y: 0.75 - 0.7 * toNumber(if i < n then i else i - n) / toNumber (if i < n then n else m)
            }
    ,   edges: repeat2 n m \i j → i ↔ (j + n)
    }

-- | generate a sun graph
sun ∷ Int → Graph
sun n =
    {   vertices: repeat (2*n) \i → 
            {   x: 0.50 + (if even i then 0.2 else 0.4) * cos (toNumber i * 2.0 * pi / toNumber (2*n))
            ,   y: 0.46 + (if even i then 0.2 else 0.4) * sin (toNumber i * 2.0 * pi / toNumber (2*n))
            }
    ,   edges: repeat2 (2*n) (2*n) (↔) 
                # filter \(i ↔ j) → i < j && (i + 1 == j || (even i && even j) || (i == 0 && j == 2*n-1)) 
    }

-- | représentation d'un graphe par liste d'adjacence
type AdjGraph = Array (Array Int)

addEdge ∷ Edge → AdjGraph → AdjGraph
addEdge (u ↔ v) graph = graph # ix u %~ (_ `snoc` v) # ix v %~ (_ `snoc` u)

edgesToGraph ∷ Int → Array Edge → AdjGraph
edgesToGraph n = foldr addEdge (replicate n [])

foreign import data Arena ∷ Type
foreign import makeEDSAux ∷ AdjGraph → String → Int → Arena
foreign import guardsAnswerAux ∷ Maybe Int → (Int → Maybe Int) → Arena → Array Int → Int → Maybe (Array Int)
foreign import attackerAnswerAux ∷ Maybe Int → (Int → Maybe Int) → Arena → Array Int → Maybe Int

guardsAnwser ∷ AdjGraph → Arena → Array Int → Int → Maybe (Array Int)
guardsAnwser graph arena guards attack = guardsAnswerAux Nothing Just arena guards attack >>= goodPermutation graph guards 

attackerAnswer ∷ Arena → Array Int → Maybe Int
attackerAnswer = attackerAnswerAux Nothing Just

makeEDS ∷ Int → Array Edge → Rules → Int → Arena
makeEDS n edges rules = makeEDSAux (edgesToGraph n edges) (if rules == OneGuard then "one" else "many")

hasEdge ∷ Int → Int → AdjGraph → Boolean
hasEdge u v graph = maybe false (elem v) (graph !! u)

permutations ∷ Array Int → Array (Array Int)
permutations t = case uncons t of
    Nothing → [[]]
    Just {head, tail} → do
        p ← permutations tail
        i ← 0 .. (length tail)
        maybe [] pure (insertAt i head p)

goodPermutation ∷ AdjGraph → Array Int → Array Int → Maybe (Array Int)
goodPermutation graph guards answer =
    permutations answer
        # filter (\guards2 → and (zipWith (\x y → x == y || hasEdge x y graph) guards guards2))
        # minimumBy (comparing \guards2 → zipWith (\x y → x /= y) guards guards2 # filter identity # length)


---- définition du State

type Ext' = 
    {   graph ∷ Graph
    ,   nextmove ∷ Array Int
    ,   phase ∷ Phase
    ,   graphkind ∷ GraphKind
    ,   draggedGuard ∷ Maybe Int
    ,   rules ∷ Rules
    ,   arena ∷ Maybe Arena
    }

newtype ExtState = Ext Ext'

type State = GState Position ExtState

---- définition des lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_graph ∷ Lens' State Graph
_graph = _ext' ∘ lens _.graph _{graph = _}
_nextmove ∷ Lens' State (Array Int)
_nextmove = _ext' ∘ lens _.nextmove _{nextmove = _}
_phase ∷ Lens' State Phase
_phase = _ext' ∘ lens _.phase _{phase = _}
_graphkind ∷ Lens' State GraphKind
_graphkind = _ext' ∘ lens _.graphkind _{graphkind = _}
_rules ∷ Lens' State Rules
_rules = _ext' ∘ lens _.rules _{rules = _}
_arena ∷ Lens' State (Maybe Arena)
_arena = _ext' ∘ lens _.arena _{arena = _}
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
            _{nbRows = 6, customSize = true, mode = DuelMode}
            (Ext { graphkind: Path, graph: path 1, nextmove: [], phase: PrepPhase, draggedGuard: Nothing, rules: OneGuard, arena: Nothing})


isValidNextMove ∷ State → Array Int → Boolean
isValidNextMove st dests =
    case (st^._position).attacked of
        Nothing → false
        Just attack →
            let edges = (st^._graph).edges
                srcs = (st^._position).guards
                moveEdges = zipWith (↔) srcs dests
            in
            elem attack dests
            && (moveEdges # all \edge@(from↔to) -> from == to || elem edge edges)
            && length (nub dests) == length dests

-- fonction déclenchée entre le passage de la phase de préparation à celle de jeu
startGame ∷ State → State
startGame st = st  
                # _phase .~ GamePhase 
                # _nextmove .~ (st^._position).guards
                # _arena %~ \arena →
                                if isJust arena then
                                    arena
                                else if st^._mode == DuelMode then
                                    Nothing 
                                else 
                                    Just $ makeEDS 
                                        (length (st^._graph).vertices)
                                        (st^._graph).edges
                                        (st^._rules)
                                        (length (st^._position).guards)


instance game ∷ Game {guards ∷ Array Int, attacked ∷ Maybe Int} ExtState Move where
    name _ = "eternal"

    play state (Defense nextmove) =
        case (state^._position).attacked of
            Just attacked →
                if isValidNextMove state nextmove then
                    Just {attacked: Nothing, guards: nextmove}
                else
                    Nothing
            _ → Nothing

    play state (Attack x) =
        if elem x (state^._position).guards || isJust (state^._position).attacked then
            Nothing
        else
            Just $ (state^._position) { attacked = Just x}

    initialPosition _ = pure {guards: [], attacked: Nothing}
    
    onNewGame state =
        let state2 = state 
                        # _nextmove .~ [] 
                        # _phase .~ PrepPhase 
                        # _draggedGuard .~ Nothing
                        # _arena .~ Nothing
        in
        case state^._graphkind of
            Path → pure (state2 # _graph .~ path (state^._nbRows))
            Cycle → pure (state2 # _graph .~ cycle (state^._nbRows))
            Grid → pure (state2 # _graph .~ grid (state^._nbRows) (state^._nbColumns))
            Biclique → pure (state2 # _graph .~ biclique (state^._nbRows) (state^._nbColumns))
            Sun → pure (state2 # _graph .~ sun (state^._nbRows))

    isLevelFinished state =
        let guards = (state^._position).guards 
            edges = (state^._graph).edges
        in
        case (state^._position).attacked of
            Nothing → false
            Just attack →
                guards # all \guard → not (elem (guard ↔ attack) edges)

    computerMove st
        | isLevelFinished st = pure Nothing
        | otherwise = case (st^._arena ∧ (st^._position).attacked) of
            Just arena ∧ Just attack → pure $ Defense <$> guardsAnwser
                                                            (edgesToGraph
                                                                (length (st^._graph).vertices)
                                                                (st^._graph).edges
                                                            )
                                                            arena
                                                            (st^._position).guards
                                                            attack
            Just arena ∧ Nothing →
                case attackerAnswer arena (st^._position).guards of
                    Just attack → pure $ Just (Attack attack)
                    Nothing → (0 .. (length (st^._graph).vertices - 1)) 
                                # filter (\i → not (elem i (st^._position).guards))
                                # R.element'
                                <#> map Attack
            _ → pure Nothing

    onPositionChange = startGame

    sizeLimit st = case st^._graphkind of
        Grid → SizeLimit 2 2 6 6
        Sun → SizeLimit 3 0 6 0
        Biclique → SizeLimit 1 1 6 6
        _ → SizeLimit 3 0 11 0

    updateScore st = st ∧ true
    saveToJson _ = Nothing
    loadFromJson st _ = st

toggleGuard ∷ Int → Array Int → Array Int
toggleGuard x l = if elem x l then filter (_ /= x) l else l `snoc` x

addToNextMove ∷ Array Edge → Int → Int → Array Int → Array Int → Array Int
addToNextMove edges from to srcs dests
    | from == to || elem (from ↔ to) edges =
        case elemIndex from srcs of
            Nothing → dests
            Just i → dests # ix i .~ to 
    | otherwise = dests

dragGuard ∷ Maybe Int → State → State
dragGuard to st =
    case st^._draggedGuard of
        Nothing → st
        Just from →
            let to2 = fromMaybe from to in
            st # _nextmove %~ addToNextMove (st^._graph).edges from to2 (st^._position).guards  # _draggedGuard .~ Nothing

data Msg = Core CoreMsg | SetGraphKind GraphKind | SetRules Rules 
            | DragGuard Int | DropGuard Int | LeaveGuard | DropOnBoard
            | StartGame | MoveGuards | ToggleGuard Int | Play Int
instance withcore ∷ MsgWithCore Msg where core = Core
    
update ∷ Msg → Update State EFFS
update (Core msg) = coreUpdate msg
update (SetGraphKind kind) = newGame ((_graphkind .~ kind) <<< ( 
                                        case kind of
                                            Grid → (_nbRows .~ 3) <<< (_nbColumns .~ 3)
                                            Biclique → (_nbRows .~ 4) <<< (_nbColumns .~ 1)
                                            Sun → (_nbRows .~ 3) <<< (_nbColumns .~ 0)
                                            _ → (_nbRows .~ 6) <<< (_nbColumns .~ 0)
                                    ))
update (SetRules rules) = newGame (_rules .~ rules)
update StartGame = modify startGame
update MoveGuards = do
    st ← get
    playA $ Defense (st ^. _nextmove)
update (ToggleGuard x) = pure unit
update (DragGuard x) = modify $ _draggedGuard .~ Just x
update (DropGuard to) = modify $ dragGuard (Just to)
update LeaveGuard = modify $ _draggedGuard .~ Nothing
update DropOnBoard = modify $ dragGuard Nothing

update (Play x) = do
    st ← get
    let guards = (st^._position).guards
    case st^._phase ∧ (st^._position).attacked  of
        PrepPhase ∧ _ → modify $ _position ∘ _guards %~ toggleGuard x
        GamePhase ∧ Just attacked → playA $ Defense (addToNextMove (st^._graph).edges x attacked guards guards)
        GamePhase ∧ Nothing → playA (Attack x)