module Game.Eternal.Model where

import MyPrelude

import Effect.Class (liftEffect)
import Game.Common (releasePointerCapture)
import Game.Core (class Game, class MsgWithCore, CoreMsg, GState, SizeLimit(..), Mode(..), Dialog(..),
                   playA, coreUpdate, _ext, genState, newGame, isLevelFinished,
                   _position, _mode, _nbRows, _nbColumns, _dialog)
import Lib.Graph (Graph, Edge, (↔))
import Lib.Random as R
import Lib.Update (Update, get, modify_)
import Lib.Util (repeat2)
import UI.GraphEditor as GEditor
import Web.Event.Event (stopPropagation)
import Web.PointerEvent.PointerEvent (PointerEvent, toEvent)


data GraphKind = Path | Cycle | Grid | Biclique | CustomGraph
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
    {   title: "Chemin"
    ,   vertices: repeat n \i → 
            {   x: 0.50 + 0.35 * cos (toNumber i * 2.0 * pi / toNumber n)
            ,   y: 0.50 + 0.35 * sin (toNumber i * 2.0 * pi / toNumber n)
            }
    ,   edges: repeat (n - 1) \i → i ↔ (i + 1)
    }

-- | generate a cycle graph
cycle ∷ Int → Graph
cycle n = g { title = "Cycle", edges = g.edges `snoc` (0 ↔ (n-1)) }
    where g = path n

-- | generate a grid graph
grid ∷ Int → Int → Graph
grid n m =
    let p = max n m in
    {   title: "Grille"
    ,   vertices: repeat2 n m \i j →
            {   x: 0.15 + 0.7 * toNumber i / toNumber (p-1)
            ,   y: 0.1 + 0.7 * toNumber j / toNumber (p-1)
            }
    ,   edges: (repeat2 n (m-1) \i j → (i*m+j) ↔ (i*m+j+1))
                <> (repeat2 (n-1) m \i j → (i*m+j) ↔ (i*m+j+m))
    }

-- | generate a star graph
star ∷ Int → Graph
star n =
    {   title: "Etoile"
    ,   vertices: {x: 0.5, y: 0.5} `cons` repeat (n-1) \i → 
            {   x: 0.50 + 0.35 * cos (toNumber i * 2.0 * pi / toNumber (n-1))
            ,   y: 0.50 + 0.35 * sin (toNumber i * 2.0 * pi / toNumber (n-1))
            }
    ,   edges: repeat (n - 1) \i → (i + 1) ↔ 0
    }

-- | generate a sun graph
biclique ∷ Int → Int → Graph
biclique m n =
    {   title: "Biclique"
    ,   vertices: repeat (n+m) \i → 
            {   x: if i < n then 0.2 else 0.8
            ,   y: 0.75 - 0.7 * toNumber(if i < n then i else i - n) / toNumber (if i < n then n else m)
            }
    ,   edges: repeat2 n m \i j → i ↔ (j + n)
    }

-- | generate a sun graph
sun ∷ Int → Graph
sun n =
    {   title: "Soleil"
    ,   vertices: repeat (2*n) \i → 
            {   x: 0.50 + (if even i then 0.2 else 0.4) * cos (toNumber i * 2.0 * pi / toNumber (2*n))
            ,   y: 0.46 + (if even i then 0.2 else 0.4) * sin (toNumber i * 2.0 * pi / toNumber (2*n))
            }
    ,   edges: repeat2 (2*n) (2*n) (↔) 
                # filter \(i ↔ j) → i < j && (i + 1 == j || (even i && even j) || (i == 0 && j == 2*n-1)) 
    }

-- | représentation d'un graphe par liste d'adjacence
type AdjGraph = Array (Array Int)

addEdge ∷ Edge → AdjGraph → AdjGraph
addEdge (u ↔ v) graph = graph 
                        # over (ix u) (_ `snoc` v) 
                        # over (ix v) (_ `snoc` u)

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
        # minimumBy (comparing \guards2 → zipWith (≠) guards guards2 # filter identity # length)


---- définition du State

type Ext' = 
    {   graph ∷ Graph
    ,   nextmove ∷ Array Int
    ,   phase ∷ Phase
    ,   graphkind ∷ GraphKind
    ,   draggedGuard ∷ Maybe Int
    ,   rules ∷ Rules
    ,   arena ∷ Maybe Arena
    ,   geditor ∷ GEditor.Model
    }

newtype ExtState = Ext Ext'

type State = GState Position ExtState

---- définition des lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_graph ∷ Lens' State Graph
_graph = _ext' ∘ prop (Proxy ∷ _ "graph")
_nextmove ∷ Lens' State (Array Int)
_nextmove = _ext' ∘ prop (Proxy ∷ _ "nextmove")
_phase ∷ Lens' State Phase
_phase = _ext' ∘ prop (Proxy ∷ _ "phase")
_graphkind ∷ Lens' State GraphKind
_graphkind = _ext' ∘ prop (Proxy ∷ _ "graphkind")
_rules ∷ Lens' State Rules
_rules = _ext' ∘ prop (Proxy ∷ _ "rules")
_arena ∷ Lens' State (Maybe Arena)
_arena = _ext' ∘ prop (Proxy ∷ _ "arena")
_draggedGuard ∷ Lens' State (Maybe Int)
_draggedGuard = _ext' ∘ prop (Proxy ∷ _ "draggedGuard")
_geditor ∷ Lens' State GEditor.Model
_geditor = _ext' ∘ prop (Proxy ∷ _ "geditor")

_guards ∷ Lens' Position (Array Int)
_guards = prop (Proxy ∷ _ "guards")
_attacked ∷ Lens' Position (Maybe Int)
_attacked = prop (Proxy ∷ _ "attacked")

-- | état initial
istate ∷ State
istate = genState 
            {guards: [], attacked: Nothing}
            _{nbRows = 6, customSize = true, mode = DuelMode}
            (Ext { graphkind: Path
                 , graph: path 1
                 , nextmove: []
                 , phase: PrepPhase
                 , draggedGuard: Nothing
                 , rules: OneGuard
                 , arena: Nothing
                 , geditor: GEditor.init
                 })

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
            && (moveEdges # all \edge@(from↔to) → from == to || elem edge edges)
            && length (nub dests) == length dests

-- fonction déclenchée entre le passage de la phase de préparation à celle de jeu
startGame ∷ State → State
startGame st = st
                # set _phase GamePhase 
                # set _nextmove (st^._position).guards
                # over _arena \arena →
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


instance Game Position ExtState Move where
    name _ = "eternal"

    play state (Defense nextmove) =
        case (state^._position).attacked of
            Just _ →
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
                        # set _nextmove [] 
                        # set _phase PrepPhase 
                        # set _draggedGuard Nothing
                        # set _arena Nothing
        in
        case state^._graphkind of
            Path → pure $ state2 # set _graph (path $ state^._nbRows)
            Cycle → pure $ state2 # set _graph (cycle $ state^._nbRows)
            Grid → pure $ state2 # set _graph (grid (state^._nbRows) (state^._nbColumns))
            Biclique → pure $ state2 # set _graph (biclique (state^._nbRows) (state^._nbColumns))
            -- Sun → pure $ state2 # set _graph (sun $ state^._nbRows)
            CustomGraph → pure state2

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
        | otherwise = case st^._arena, (st^._position).attacked of
            Just arena, Just attack →
                let graph = edgesToGraph (length (st^._graph).vertices) (st^._graph).edges
                    answer = guardsAnwser graph arena (st^._position).guards attack
                in
                pure $ Defense <$> answer
            Just arena, Nothing →
                case attackerAnswer arena (st^._position).guards of
                    Just attack → pure $ Just (Attack attack)
                    Nothing → (0 .. (length (st^._graph).vertices - 1)) 
                                # filter (\i → not (elem i (st^._position).guards))
                                # R.element'
                                <#> map Attack
            _, _ → pure Nothing

    onPositionChange = startGame

    sizeLimit st = case st^._graphkind of
        Grid → SizeLimit 2 2 6 6
        -- Sun → SizeLimit 3 0 6 0
        Biclique → SizeLimit 1 1 6 6
        CustomGraph → SizeLimit 0 0 0 0
        _ → SizeLimit 3 0 11 0

    updateScore st = st ∧ true
    saveToJson _ = Nothing
    loadFromJson st _ = st

toggleGuard ∷ Int → Array Int → Array Int
toggleGuard x l = if elem x l then filter (_ ≠ x) l else l `snoc` x

addToNextMove ∷ Array Edge → Int → Int → Array Int → Array Int → Array Int
addToNextMove edges from to srcs dests
    | from == to || elem (from ↔ to) edges =
        case elemIndex from srcs of
            Nothing → dests
            Just i → dests # set (ix i) to 
    | otherwise = dests

dragGuard ∷ Maybe Int → State → State
dragGuard to st =
    case st^._draggedGuard of
        Nothing → st
        Just from →
            let to2 = fromMaybe from to in
            st # over _nextmove (addToNextMove (st^._graph).edges from to2 (st^._position).guards)
               # set _draggedGuard Nothing

data Msg = Core CoreMsg 
        | GEditor GEditor.Msg
        | SetGraphKind GraphKind
        | SetRules Rules 
        | DragGuard Int PointerEvent
        | DropGuard Int PointerEvent
        | LeaveGuard
        | DropOnBoard
        | StartGame
        | MoveGuards
        | ToggleGuard Int
        | Play Int
        | CloseEditor Graph
        | NoAction
instance MsgWithCore Msg where core = Core
instance GEditor.MsgWithGEditor Msg where geditormsg = GEditor    

update ∷ Msg → Update State Unit
update (Core msg) = coreUpdate msg
update (GEditor msg) = GEditor.update _geditor msg
update (SetGraphKind kind) = newGame $ set _graphkind kind >>>
                                        case kind of
                                            Grid → set _nbRows 3 ∘ set _nbColumns 3
                                            Biclique → set _nbRows 4 ∘ set _nbColumns 1
                                            --Sun → set _nbRows 3 ∘ set _nbColumns 0
                                            CustomGraph → set _dialog CustomDialog
                                            _ → set _nbRows 6 ∘ set _nbColumns 0

update (SetRules rules) = newGame $ set _rules rules
update StartGame = modify_ startGame
update MoveGuards = do
    st ← get
    playA $ Defense (st ^. _nextmove)
update (ToggleGuard _) = pure unit
update (DragGuard x ev) = do
    liftEffect $ releasePointerCapture ev
    _draggedGuard .= Just x
update (DropGuard to ev) = do
    liftEffect $ stopPropagation $ toEvent ev
    modify_ $ dragGuard (Just to)
update LeaveGuard = _draggedGuard .= Nothing
update DropOnBoard = modify_ $ dragGuard Nothing
update (Play x) = do
    st ← get
    let guards = (st^._position).guards
    case st^._phase, (st^._position).attacked  of
        PrepPhase, _ → modify_ $ over (_position ∘ _guards) (toggleGuard x)
        GamePhase, Just attacked → playA $ Defense (addToNextMove (st^._graph).edges x attacked guards guards)
        GamePhase, Nothing → playA (Attack x)
update (CloseEditor g) = modify_ $ (_dialog .~ NoDialog) >>> (_graph .~ g)
update NoAction = pure unit