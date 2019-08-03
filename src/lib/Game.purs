module Lib.Game where

import Prelude
import Data.Maybe (Maybe, maybe, fromMaybe)
import Data.Array (snoc)
import Data.Array.NonEmpty (fromArray, head, init, last) as NArray

data State cls pos ext = St {
    position :: pos,
    history :: Array pos,
    redoHistory :: Array pos,
    newState :: Maybe (State cls pos ext)
    | ext
}

class Game cls pos ext mov | cls -> pos mov ext where
    play :: State cls pos ext -> mov -> pos
    canPlay :: State cls pos ext -> mov -> Boolean
    initialPosition :: State cls pos ext -> pos
    isLevelFinished :: State cls pos ext -> Boolean

undo :: forall cls pos ext mov. State cls pos ext -> State cls pos ext
undo (St state) = fromMaybe (St state) $ do
    hs <- NArray.fromArray state.history
    pure $ St state {
        position = NArray.last hs,
        history = NArray.init hs,
        redoHistory = state.redoHistory `snoc` state.position 
    }

redo :: forall cls pos ext mov. State cls pos ext -> State cls pos ext
redo (St state) = fromMaybe (St state) $ do
    hs <- NArray.fromArray state.redoHistory
    pure $ St state {
        position = NArray.last hs,
        redoHistory = NArray.init hs,
        history = state.history `snoc` state.position 
    }

reset :: forall cls pos ext mov. State cls pos ext -> State cls pos ext
reset (St state) = fromMaybe (St state) $ do
    hs <- NArray.fromArray state.history
    pure $ St state {
        position = NArray.head hs,
        history = [],
        redoHistory = []
    }

_play :: forall cls pos ext mov. Game cls pos ext mov =>
    {move :: mov, pushToHistory :: Boolean} -> State cls pos ext -> State cls pos ext
_play {move, pushToHistory} st@(St state) =
    if canPlay st move then
        St state {
            position = play st move,
            history = state.history `snoc` state.position ,
            redoHistory = []
        }
    else
        st

_play' :: forall cls pos ext mov. Game cls pos ext mov =>
    mov -> State cls pos ext -> State cls pos ext
_play' move = _play {move, pushToHistory: true}

newGame :: forall cls pos ext mov. Game cls pos ext mov =>
    (State cls pos ext -> State cls pos ext) -> State cls pos ext -> State cls pos ext
newGame f state =
    let st2@(St state2) = f state in
    St state2{position = initialPosition st2}

newGame' :: forall a cls pos ext mov. Game cls pos ext mov =>
    (a -> State cls pos ext -> State cls pos ext) -> a -> State cls pos ext -> State cls pos ext
newGame' f val state =
    let st2@(St state2) = f val state in
    St state2{position = initialPosition st2}
