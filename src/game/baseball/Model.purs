module Game.Baseball.Model where

import Prelude
import Data.Array ((!!), elem, mapWithIndex, all, (..))
import Data.Maybe (Maybe(Nothing), maybe)
import Lib.Core (swap)
import Lib.Random (Random, shuffle)
import Lib.Game (class Game, State(..), newGame', Dialog (NoDialog, Rules), Mode (SoloMode))

type Position = Array Int
data BaseballCls = BaseballCls
type BaseballState = State BaseballCls Position (nbBases :: Int)

example :: BaseballState
example = St {
    position: [],
    nbBases: 5,

    history: [],
    redoHistory: [],
    dialog: Rules,
    levelFinished: false,
    turn: 0,
    mode: SoloMode
}

instance baseballGame :: Game BaseballCls (Array Int) (nbBases :: Int) Int where
    play (St st) i = st.position # swap 0 i
    canPlay (St st) i = (do
        x <- st.position !! 0
        y <- st.position !! i
        pure (x / 2 - y / 2)
    ) # maybe false (\x -> elem x [1, st.nbBases-1, -1, 1-st.nbBases])
    initialPosition (St {nbBases}) = shuffle (0 .. (2 * nbBases - 1))
    isLevelFinished (St st) =
        st.position # mapWithIndex (\i j -> i / 2 == j / 2) # all identity

setNbBases :: Int -> BaseballState -> Random BaseballState
setNbBases = newGame'(_setNbBases)
    where _setNbBases i (St s) = St s{nbBases = i}
