module Game.Baseball.Model where

import Prelude
import Data.Array ((!!), elem, mapWithIndex, all, (..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Lens (lens, Lens', set, (^.), (.~))
import Lib.Util (swap)
import Lib.Random (shuffle, randomInt)
import Pha.Action (Action)
import Game.Core (class Game, State(..), genState, newGame', _position, defaultSizeLimit)
infixr 9 compose as ∘

type Position = Array Int
type Ext' = { nbBases :: Int, missingPeg :: Int }
newtype ExtState = Ext Ext'
type BaseballState = State Position ExtState

_ext :: Lens' BaseballState Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_nbBases :: Lens' BaseballState Int
_nbBases = _ext ∘ lens (_.nbBases) (_{nbBases = _})
_missingPeg :: Lens' BaseballState Int
_missingPeg = _ext ∘ lens (_.missingPeg) (_{missingPeg = _})

baseballState :: BaseballState
baseballState = genState [] identity (Ext { nbBases: 5, missingPeg: 0 })

instance baseballGame :: Game (Array Int) ExtState Int where
    play state i = state^._position # swap (state^._missingPeg) i
    canPlay state i = fromMaybe false $ do
        let position = state^._position
        let nbBases = state^._nbBases
        x <- position !! (state^._missingPeg)
        y <- position !! i
        let diff = x / 2 - y / 2
        pure $ elem diff [1, nbBases-1, -1, 1-nbBases]

    initialPosition state = shuffle $ 0 .. (2 * state^._nbBases - 1)
    isLevelFinished state = state^._position # mapWithIndex (\i j -> i / 2 == j / 2) # all identity
    onNewGame state = randomInt (2 * state^._nbBases) <#> \i -> state # _missingPeg .~ i  
    
    computerMove = const Nothing
    sizeLimit = defaultSizeLimit
    
setNbBases :: Int -> Action BaseballState
setNbBases = newGame' (set _nbBases)
