module Game.Tricolor.Model where

import MyPrelude
import Data.FoldableWithIndex (allWithIndex)
import Lib.Util (abs, (..))
import Lib.Random (shuffle, randomInt)
import Pha.Action (Action, RNG, setState)
import Game.Core (class Game, GState(..), genState, newGame', _position, defaultSizeLimit)

type Ext' = { 
    size :: Int,
    nbColors :: Int,
    range :: Int,
    hoverCell :: Maybe Int
}
newtype ExtState = Ext Ext'
type State = GState (Array Int) ExtState

-- lenses
_ext :: Lens' State Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_size :: Lens' State Int
_size = _ext ∘ lens (_.size) (_{size = _})
_nbColors :: Lens' State Int
_nbColors = _ext ∘ lens (_.nbColors) (_{nbColors = _})
_range :: Lens' State Int
_range = _ext ∘ lens (_.range) (_{range = _})
_hoverCell :: Lens' State (Maybe Int)
_hoverCell = _ext ∘ lens (_.hoverCell) (_{hoverCell = _})

-- état initial
istate :: State
istate = genState [] identity (Ext { size: 5, nbColors: 2, range: 1, hoverCell: Nothing })

inRange :: State -> Int -> Int -> Boolean
inRange state i i' = min diff (state^._size - diff) <= state^._range
    where diff = abs (i - i')
    
instance tricolorGame :: Game (Array Int) ExtState Int where
    play state i = state^._position # mapWithIndex \i' color ->
        if inRange state i i' then
            (color + 1) `mod` (state^._nbColors)
        else
            color
    canPlay _ _ = true
    initialPosition state = sequence $ replicate (state^._size) $ randomInt (state^._nbColors)
    isLevelFinished state = state^._position # all (eq 0)

    onNewGame = pure
    computerMove = const Nothing
    sizeLimit = defaultSizeLimit
    updateScore st = st ~ true 
    
setSizeA :: ∀effs. Int -> Action State (rng :: RNG | effs)
setSizeA = newGame' (set _size)

setNbColorsA :: ∀effs. Int -> Action State (rng :: RNG | effs)
setNbColorsA = newGame' (set _nbColors)

setRangeA :: ∀effs. Int -> Action State (rng :: RNG | effs)
setRangeA = newGame' (set _range)

setHoverCellA :: ∀effs. Maybe Int -> Action State effs
setHoverCellA i = setState (_hoverCell .~ i)