module Game.Nim.Model where

import Prelude
import Data.Tuple (Tuple (Tuple), fst, snd)
import Data.Maybe (maybe)
import Data.Traversable (sequence)
import Data.Array ((!!), filter, sortWith, all, replicate, foldr)
import Data.Int.Bits (xor)
import Data.Lens (lens, Lens', view, set, (^.), (%~))
import Data.Lens.Index (ix)
import Lib.Core (tabulate2)
import Lib.Random (RandomFn, randomInt)
import Game.Core (class Game, class TwoPlayersGame, State (..), Mode(..),
                genState, newGame', canPlay, _position, _turn, computerMove', defaultSizeLimit, defaultOnNewGame)

data Move = Move Int Int
type Ext' = { nbPiles :: Int, length :: Int }
newtype ExtState = Ext Ext'
type NimState = State (Array (Tuple Int Int)) ExtState

nimState :: NimState
nimState = genState [] (_{mode = ExpertMode }) (Ext { length: 10, nbPiles: 4 })

_ext :: Lens' NimState Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))

_length :: Lens' NimState Int
_length = _ext <<< lens (_.length) (_{length = _})

_nbPiles :: Lens' NimState Int
_nbPiles = _ext <<< lens (_.nbPiles) (_{nbPiles = _})

instance nimGame :: Game (Array (Tuple Int Int)) ExtState Move where
    canPlay state (Move pile pos) =
        flip(maybe false) (state^._position !! pile)
            \(Tuple p1 p2) -> pos /= p1 && pos /= p2 && if state^._turn == 0 then pos < p2 else pos > p1

    play state (Move pile pos) = 
        state ^. _position # (ix pile) %~
            \(Tuple p1 p2) -> if state^._turn == 0 then Tuple pos p2 else Tuple p1 pos
    
    isLevelFinished state = state^._position # all
        \(Tuple p1 p2) -> p2 - p1 == 1 && p1 == (if state^._turn == 1 then state^._length - 2 else 0)

    initialPosition state = 
        sequence $ replicate (state^._nbPiles) $ do
            x <- randomInt 5
            y <- randomInt 5
            pure $ Tuple x (y + 5)

    computerMove = computerMove'
    sizeLimit = defaultSizeLimit
    onNewGame = defaultOnNewGame

instance nimGame2 :: TwoPlayersGame (Array (Tuple Int Int)) ExtState Move where
    possibleMoves state =
        tabulate2 (state^._nbPiles) (state^._length) (\{row, col} -> Move row col)
        # filter (canPlay state)
        # sortWith \(Move pile pos) -> flip (maybe 0)
            (state^._position !! pile)
            \x -> if state^._turn == 0 then fst x - pos else pos - snd x

    isLosingPosition = view _position >>> foldr (\t -> xor (snd t - fst t - 1)) 0 >>> eq 0

setNbPiles :: Int -> RandomFn NimState
setNbPiles = newGame' (set _nbPiles)

setLength :: Int -> RandomFn NimState
setLength = newGame'(set _length)
