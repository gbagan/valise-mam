module Game.Noirblanc.Model where
import Prelude
import Data.Tuple (Tuple (Tuple))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Data.Array ((!!), replicate, mapWithIndex, all, foldr, modifyAtIndices)
import Optic.Core (Lens', lens, (^.), (.~), set)
import Lib.Random (Random, RandomFn, randomInt)
import Lib.Core (dCoords)
import Lib.Game (class Game, State (..), SizeLimit(..), _position, _nbColumns, _nbRows, newGame', genState)

type Position = { light :: Array Boolean, played :: Array Boolean }
data ExtState = Ext {
    mode2 :: Int,
    level :: Int
}
type NoirblancState = State Position ExtState

noirblancState :: NoirblancState
noirblancState = genState {light: [], played: []} identity (Ext { level: 0, mode2: 0 })

_mode2 :: Lens' NoirblancState Int
_mode2 = lens (\(State _ (Ext s)) -> s.mode2) (\(State s (Ext ext)) x -> State s (Ext ext{mode2 = x}))
_level :: Lens' NoirblancState Int
_level = lens (\(State _ (Ext s)) -> s.level) (\(State s (Ext ext)) x -> State s (Ext ext{level = x}))


neighbor :: NoirblancState -> Int -> Int -> Boolean
neighbor state index1 index2 =
    row * row + col * col == 1
    || mode `mod` 3 == 0 && index1 == index2 
    || mode >= 2 && index1 /= index2 && row * col == 0
    where
        mode = state^._mode2
        {row, col} = dCoords (state^._nbColumns) index1 index2
    
toggleCell :: NoirblancState -> Int -> Array Boolean -> Array Boolean
toggleCell state index = mapWithIndex \i color -> color /= neighbor state index i

genRandomBoard :: NoirblancState -> Random (Array Boolean)
genRandomBoard state = do
    let size = state^._nbRows * state^._nbColumns
    nbMoves <- randomInt (size + 1)
    rints <- sequence $ replicate nbMoves (randomInt size)
    pure $ foldr (toggleCell state) (replicate size true) rints


instance jetonsGame :: Game { light :: Array Boolean, played :: Array Boolean } ExtState Int where
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

selectMode :: Int -> RandomFn NoirblancState
selectMode = newGame' \mode state -> state # _mode2 .~ mode # _level .~ 0
selectLevel :: Int -> RandomFn NoirblancState
selectLevel = newGame' (set _level)

{-
    state: {
        mode: 0, // [0, 1, 2, 3]
        level: 0,
        maxLevels: [0, 1, 1, 0],
        help: false,
        keysequence: [],
    },

    actions: $ => ({
        
        
        whenLevelFinished: state =>
            max(
                state.level >= 4 ?
                    6
                :
                    state.level + ([0, 3].includes(state.mode) ? 1 : 2)
            )
            |> (nextLevel =>
                state
                |> set(['maxLevels', state.mode], nextLevel)
                |> $.newGame({level: lvl => Math.min(lvl + 1, 6)}, {noDialog: true})
            ),
        keyDown: konamiCode(set('maxLevels', [6, 6, 6, 6])),
    }),

    computed: state => ({ customSize: state.level >= 5 })
});
-}