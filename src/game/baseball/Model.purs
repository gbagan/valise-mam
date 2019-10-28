module Game.Baseball.Model where

import Prelude
import Data.Array ((!!), elem, mapWithIndex, all, (..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Lens (lens, Lens', set, (^.))
import Lib.Core (swap)
import Lib.Random (shuffle)
import Pha.Class (Action)
import Game.Core (class Game, State(..), genState, newGame', _position, defaultSizeLimit, defaultOnNewGame)

type Position = Array Int
type Ext' = { nbBases :: Int }
newtype ExtState = Ext Ext'
type BaseballState = State Position ExtState

_ext :: Lens' BaseballState Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_nbBases :: Lens' BaseballState Int
_nbBases = _ext <<< lens (_.nbBases) (_{nbBases = _})

baseballState :: BaseballState
baseballState = genState [] identity (Ext { nbBases: 5 })

instance baseballGame :: Game (Array Int) ExtState Int where
    play state i = state^._position # swap 0 i
    canPlay state i = fromMaybe false $ do
        let position = state^._position
        let nbBases = state^._nbBases
        x <- position !! 0
        y <- position !! i
        let diff = x / 2 - y / 2
        pure $ elem diff [1, nbBases-1, -1, 1-nbBases]

    initialPosition state = shuffle $ 0 .. (2 * state^._nbBases - 1)
    isLevelFinished state = state^._position # mapWithIndex (\i j -> i / 2 == j / 2) # all identity
    computerMove state = Nothing
    sizeLimit = defaultSizeLimit
    onNewGame = defaultOnNewGame

setNbBases :: Int -> Action BaseballState
setNbBases = newGame' (set _nbBases)
