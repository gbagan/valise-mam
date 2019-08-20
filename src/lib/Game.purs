module Lib.Game where

import Prelude
import Data.Maybe (Maybe, fromMaybe)
import Data.Array (snoc, null)
import Data.Array.NonEmpty (fromArray, head, init, last) as NArray
import Lib.Random (Random)

data Dialog a = Rules | NoDialog | ConfirmNewGame a
data Mode = SoloMode | RandomMode | ExpertMode | DuelMode

data State cls pos ext = St {
    position :: pos,
    history :: Array pos,
    redoHistory :: Array pos,
    levelFinished :: Boolean,
    dialog :: Dialog (State cls pos ext),
    turn :: Int,
    mode :: Mode
    | ext
}

class Game cls pos ext mov | cls -> pos mov ext where
    play :: State cls pos ext -> mov -> pos
    canPlay :: State cls pos ext -> mov -> Boolean
    initialPosition :: State cls pos ext -> Random pos
    isLevelFinished :: State cls pos ext -> Boolean

changeTurn :: forall cls pos ext. State cls pos ext -> State cls pos ext
changeTurn (St state) =
    case state.mode of
        DuelMode -> St state{turn = 1 - state.turn}
        _ -> St state

undo :: forall cls pos ext. State cls pos ext -> State cls pos ext
undo (St state) = fromMaybe (St state) $ do
    hs <- NArray.fromArray state.history
    pure $ changeTurn $ St state {
        position = NArray.last hs,
        history = NArray.init hs,
        redoHistory = state.redoHistory `snoc` state.position 
    }

redo :: forall cls pos ext. State cls pos ext -> State cls pos ext
redo (St state) = fromMaybe (St state) $ do
    hs <- NArray.fromArray state.redoHistory
    pure $ changeTurn $ St state {
        position = NArray.last hs,
        redoHistory = NArray.init hs,
        history = state.history `snoc` state.position 
    }

reset :: forall cls pos ext. State cls pos ext -> State cls pos ext
reset (St state) = fromMaybe (St state) $ do
    hs <- NArray.fromArray state.history
    pure $ St state {
        position = NArray.head hs,
        history = [],
        redoHistory = [],
        turn = 0
    }

_play :: forall cls pos ext mov. Game cls pos ext mov =>
    {move :: mov, pushToHistory :: Boolean} -> State cls pos ext -> State cls pos ext
_play {move, pushToHistory} st@(St state) =
    if canPlay st move then
        let st2@(St state2) = St state {
            position = play st move,
            history = state.history `snoc` state.position ,
            redoHistory = []
        } in
        St state2{levelFinished = isLevelFinished st2}
    else
        st

_play' :: forall cls pos ext mov. Game cls pos ext mov =>
    mov -> State cls pos ext -> State cls pos ext
_play' move = _play {move, pushToHistory: true}

newGame :: forall cls pos ext mov. Game cls pos ext mov =>
    (State cls pos ext -> State cls pos ext) -> State cls pos ext -> Random (State cls pos ext)
newGame f st@(St state) =
    let st2@(St state2) = f st in do
        position <- initialPosition st2
        let st3 = St state2{position = position, history = [], redoHistory = []}
        if null state2.history then
            pure st3
        else
            pure (st # (setDialog $ ConfirmNewGame st3))

newGame' :: forall a cls pos ext mov. Game cls pos ext mov =>
    (a -> State cls pos ext -> State cls pos ext) -> a -> State cls pos ext -> Random (State cls pos ext)
newGame' f val = newGame $ f val

setDialog :: forall cls pos ext. Dialog (State cls pos ext) -> State cls pos ext -> State cls pos ext
setDialog dialog (St st) = St st{dialog = dialog}

confirmNewGame :: forall a cls pos ext. State cls pos ext -> a -> State cls pos ext
confirmNewGame st _ = setDialog NoDialog st