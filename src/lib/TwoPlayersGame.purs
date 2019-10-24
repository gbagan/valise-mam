module Lib.TwoPlayersGame where

import Prelude 
import Data.Maybe (Maybe (..))
import Data.Array.NonEmpty (fromArray)
import Lib.Random (Random, randomPick)
import Lib.Game (class Game, State)

class Game pos ext mov <= TwoPlayersGame pos ext mov | ext -> pos mov  where
    isLosingPosition :: State pos ext -> Boolean
    possibleMoves :: State pos ext -> Array mov

computerMove' :: forall pos ext mov. TwoPlayersGame pos ext mov => State pos ext -> Maybe (Random mov)
computerMove' state = 
    if isLosingPosition state then
        Nothing
    else
        randomPick <$> fromArray (possibleMoves state)
