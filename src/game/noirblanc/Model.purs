module Game.Noirblanc.Model where
import Prelude
import Data.Tuple (Tuple (Tuple))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Data.Array ((!!), replicate, mapWithIndex, all, foldr, modifyAtIndices)
import Data.Lens (Lens', lens, (^.), (.~), (%~), set)
import Data.Lens.Index (ix)
import Lib.Random (Random, randomInt)
import Lib.Util (dCoords)
import Lib.KonamiCode (konamiCode)
import Pha.Action (Action, action, asyncAction)
import Game.Core (class Game, GState(..), SizeLimit(..), playA, isLevelFinished, _position, _nbColumns, _nbRows, newGame, newGame', genState)
infixr 9 compose as ∘

type Position = { light :: Array Boolean, played :: Array Boolean }
type Ext' = {
    mode2 :: Int,
    level :: Int,
    maxLevels :: Array Int,
    keySequence :: Array String
}
newtype ExtState = Ext Ext'
type State = GState Position ExtState

istate :: State
istate = genState {light: [], played: []} identity (Ext { level: 0, mode2: 0, maxLevels: [0, 1, 1, 0], keySequence: [] })

_ext :: Lens' State Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_mode2 :: Lens' State Int
_mode2 = _ext ∘ lens (_.mode2) (_{mode2 = _})
_level :: Lens' State Int
_level = _ext ∘ lens (_.level) (_{level = _})
_maxLevels :: Lens' State (Array Int)
_maxLevels = _ext ∘ lens (_.maxLevels) (_{maxLevels = _})
_keySequence :: Lens' State (Array String)
_keySequence = _ext ∘ lens (_.keySequence) (_{keySequence = _})

neighbor :: State -> Int -> Int -> Boolean
neighbor state index1 index2 =
    row * row + col * col == 1
    || mode `mod` 3 == 0 && index1 == index2 
    || mode >= 2 && index1 /= index2 && row * col == 0
    where
        mode = state^._mode2
        {row, col} = dCoords (state^._nbColumns) index1 index2
    
toggleCell :: State -> Int -> Array Boolean -> Array Boolean
toggleCell state index = mapWithIndex \i color -> color /= neighbor state index i

genRandomBoard :: State -> Random (Array Boolean)
genRandomBoard state = do
    let size = state^._nbRows * state^._nbColumns
    nbMoves <- randomInt (size + 1)
    rints <- sequence $ replicate nbMoves (randomInt size)
    pure $ foldr (toggleCell state) (replicate size true) rints


instance noirblancGame :: Game { light :: Array Boolean, played :: Array Boolean } ExtState Int where
    play state index = { light: toggleCell state index light, played: modifyAtIndices [index] not played }
        where {light, played} = state^._position
    canPlay _ _ = true

    initialPosition state = do
        let size = state^._nbRows * state^._nbColumns
        board <- if state^._level >= 6 then genRandomBoard state else pure $ replicate size true
        pure $ { light: board, played: replicate size false }
    
    isLevelFinished state = all not (state^._position).light

    computerMove _ = Nothing
    sizeLimit _ = SizeLimit 3 3 10 10
    onNewGame state = 
        let Tuple rows columns = fromMaybe (Tuple 8 8) (sizes !! (state^._level)) in
        pure $ state # _nbRows .~ rows # _nbColumns .~ columns

sizes :: Array (Tuple Int Int)
sizes = [ Tuple 3 3, Tuple 4 4, Tuple 2 10, Tuple 3 10, Tuple 5 5, Tuple 8 8, Tuple 8 8]

selectModeA :: Int -> Action State
selectModeA mode = newGame $ (_mode2 .~ mode) ∘ (_level .~ 0)
selectLevelA :: Int -> Action State
selectLevelA = newGame' (set _level)

afterPlay :: Action State
afterPlay = asyncAction \{getState, updateState, dispatch} state ->
    let mode = state^._mode2 in
    if isLevelFinished state then do
        let nextLevel = if state^._level >= 4 then
                        6
                    else
                        state^._level + (if mode == 0 || mode == 3 then 1 else 2)
        _ <- updateState (_maxLevels ∘ ix mode .~ nextLevel)
        dispatch $ newGame (_level %~ \lvl -> min (lvl + 1) 6)
    else
        pure state

onKeyDown :: String -> Action State
onKeyDown = konamiCode _keySequence (action $ _maxLevels .~ [6, 6, 6, 6])

play2A :: Int -> Action State
play2A i = playA i <> afterPlay