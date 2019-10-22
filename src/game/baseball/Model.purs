module Game.Baseball.Model where

import Prelude
import Data.Array ((!!), elem, mapWithIndex, all, (..))
import Data.Maybe (fromMaybe)
import Optic.Core (lens, Lens', set, (^.))
import Lib.Core (swap)
import Lib.Random (Random, shuffle)
import Lib.Game (class Game, State(..), newGame', Dialog (Rules), Mode (SoloMode), _position)

type Position = Array Int
type Move = Int
data ExtState = Ext {
    nbBases :: Int
}
type BaseballState = State Position ExtState

_nbBases :: Lens' BaseballState Int
_nbBases = lens (\(State _ (Ext s)) -> s.nbBases) (\(State s (Ext ext)) x -> State s (Ext ext{nbBases = x}))

example :: BaseballState
example = State {
    position: [],
    history: [],
    redoHistory: [],
    dialog: Rules,
    levelFinished: false,
    turn: 0,
    mode: SoloMode
} (Ext { nbBases: 5 })


instance baseballGame :: Game (Array Int) ExtState Int where
    play state i = state ^. _position # swap 0 i
    canPlay state i = fromMaybe false $ do
        let position = state^._position
        let nbBases = state^._nbBases
        x <- position !! 0
        y <- position !! i
        let diff = x / 2 - y / 2
        pure $ elem diff [1, nbBases-1, -1, 1-nbBases]

    initialPosition state = shuffle $ 0 .. (2 * (state^._nbBases) - 1)
    isLevelFinished state = state^._position # mapWithIndex (\i j -> i / 2 == j / 2) # all identity

setNbBases :: Int -> BaseballState -> Random BaseballState
setNbBases = newGame'(set _nbBases)
