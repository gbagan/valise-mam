module Game.Baseball.Model where

import MyPrelude
import Data.FoldableWithIndex (allWithIndex)
import Lib.Util (swap, (..))
import Lib.Random (shuffle, randomInt)
import Pha.Action (Action, RNG)
import Game.Core (class Game, GState(..), genState, newGame', _position, defaultSizeLimit)

type Ext' = { nbBases :: Int, missingPeg :: Int }
newtype ExtState = Ext Ext'
type State = GState (Array Int) ExtState

-- lenses
_ext :: Lens' State Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_nbBases :: Lens' State Int
_nbBases = _ext ∘ lens (_.nbBases) (_{nbBases = _})
_missingPeg :: Lens' State Int
_missingPeg = _ext ∘ lens (_.missingPeg) (_{missingPeg = _})

-- état initial
istate :: State
istate = genState [] identity (Ext { nbBases: 5, missingPeg: 0 })

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
    isLevelFinished state = state^._position # allWithIndex \i j -> i / 2 == j / 2
    onNewGame state = randomInt (2 * state^._nbBases) <#> \i -> state # _missingPeg .~ i
    
    computerMove = const Nothing
    sizeLimit = defaultSizeLimit
    updateScore st = st ~ true 
    
setNbBasesA :: ∀effs. Int -> Action State (rng :: RNG | effs)
setNbBasesA = newGame' (set _nbBases)
