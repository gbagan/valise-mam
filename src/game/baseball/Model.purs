module Game.Baseball.Model where

import Prelude
import Data.Array ((!!), elem, mapWithIndex, all, updateAt, (..))
import Data.Maybe (Maybe(Nothing), maybe, fromMaybe)
import Lib.Game (class Game, State(..), newGame')

type Position = Array Int

swap :: Int -> Int -> Array Int -> Array Int
swap i j array = fromMaybe array $ do
    x <- array !! i
    y <- array !! j
    array # updateAt i y >>= updateAt j x

data BaseballCls = BaseballCls

type BaseballState = State BaseballCls (Array Int) (nbBases :: Int)

example :: BaseballState
example = St {
    position: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 
    nbBases: 5,
    history: [],
    redoHistory: [],
    newState: Nothing
}

instance baseballGame :: Game BaseballCls (Array Int) (nbBases :: Int) Int where
    play (St st) i = st.position # swap 0 i
    canPlay (St st) i = (do
        x <- st.position !! 0
        y <- st.position !! i
        pure (x / 2 - y / 2)
    ) # maybe false (\x -> [1, st.nbBases-1, -1, 1-st.nbBases] # elem x)
    initialPosition (St {nbBases}) = 0 .. (2 * nbBases - 1)
    isLevelFinished (St st) =
        st.position # mapWithIndex (\i j -> i / 2 == j / 2) # all identity


setNbBases :: Int -> BaseballState -> BaseballState
setNbBases = newGame'(_setNbBases)
    where _setNbBases i (St s) = St s{nbBases = i}
