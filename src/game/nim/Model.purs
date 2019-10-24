module Game.Nim.Model where

import Prelude
import Data.Tuple (Tuple (Tuple), fst, snd)
import Data.Maybe (maybe)
import Data.Traversable (sequence)
import Data.Array ((!!), filter, sortWith, modifyAtIndices, all, replicate, foldr)
import Data.Int.Bits (xor)
import Optic.Core (lens, Lens', view, set, (^.))
import Lib.Core (tabulate2)
import Lib.Random (Random, randomInt)
import Lib.Game (class Game, class TwoPlayersGame, State (..), Mode(..), genState, newGame', canPlay, _position, _turn, computerMove')

data Move = Move Int Int
data ExtState = Ext {
    nbPiles :: Int,
    length :: Int
}

type NimState = State (Array (Tuple Int Int)) ExtState

nimState :: NimState
nimState = genState [] (_{mode = ExpertMode }) (Ext { length: 10, nbPiles: 4 })

_length :: Lens' NimState Int
_length = lens (\(State _ (Ext s)) -> s.length) (\(State s (Ext ext)) x -> State s (Ext ext{length = x}))

_nbPiles :: Lens' NimState Int
_nbPiles = lens (\(State _ (Ext s)) -> s.nbPiles) (\(State s (Ext ext)) x -> State s (Ext ext{nbPiles = x}))

instance nimGame :: Game (Array (Tuple Int Int)) ExtState Move where
    canPlay state (Move pile pos) =
        flip(maybe false) (state^._position !! pile)
            \(Tuple p1 p2) -> pos /= p1 && pos /= p2 && if state^._turn == 0 then pos < p2 else pos > p1

    play state (Move pile pos) = 
        state^._position # modifyAtIndices [pile]
            \(Tuple p1 p2) -> if state^._turn == 0 then Tuple pos p2 else Tuple p1 pos
    
    isLevelFinished state = state^._position # all
        \(Tuple p1 p2) -> p2 - p1 == 1 && p1 == (if state^._turn == 1 then state^._length - 2 else 0)

    initialPosition state = 
        sequence $ replicate (state^._nbPiles) $ do
            x <- randomInt 5
            y <- randomInt 5
            pure $ Tuple x (y + 5)

    computerMove = computerMove'

instance nimGame2 :: TwoPlayersGame (Array (Tuple Int Int)) ExtState Move where
    possibleMoves state =
        tabulate2 (state^._nbPiles) (state^._length) (\{row, col} -> Move row col)
        # filter (canPlay state)
        # sortWith \(Move pile pos) -> flip (maybe 0)
            (state^._position !! pile)
            \x -> if state^._turn == 0 then fst x - pos else pos - snd x

    isLosingPosition = view _position >>> foldr (\t -> xor (snd t - fst t - 1)) 0 >>> eq 0

setNbPiles :: Int -> NimState -> Random NimState
setNbPiles = newGame'(set _nbPiles)

setLength :: Int -> NimState -> Random NimState
setLength = newGame'(set _length)
