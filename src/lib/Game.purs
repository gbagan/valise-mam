module Lib.Game where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Array (cons, null)
import Data.Array.NonEmpty (fromArray, head, init, last) as NArray
import Lib.Random (Random)
import Optic.Core (lens, Lens', (^.), (.~), (%~))

data Dialog a = Rules | NoDialog | ConfirmNewGame a
data Mode = SoloMode | RandomMode | ExpertMode | DuelMode

type CoreState pos ext = {
    position :: pos,
    history :: Array pos,
    redoHistory :: Array pos,
    levelFinished :: Boolean,
    dialog :: Dialog (State pos ext),
    turn :: Int,
    mode :: Mode
}

_position :: forall pos ext. Lens' (State pos ext) pos
_position = lens (\(State s _) -> s.position) (\(State s ext) x -> State s{position = x} ext)

_history :: forall pos ext. Lens' (State pos ext) (Array pos)
_history = lens (\(State s _) -> s.history) (\(State s ext) x -> State s{history = x} ext)

_redoHistory :: forall pos ext. Lens' (State pos ext) (Array pos)
_redoHistory = lens (\(State s _) -> s.redoHistory) (\(State s ext) x -> State s{redoHistory = x} ext)

_mode :: forall pos ext. Lens' (State pos ext) Mode
_mode = lens (\(State s _) -> s.mode) (\(State s ext) x -> State s{mode = x} ext)

_turn :: forall pos ext. Lens' (State pos ext) Int
_turn = lens (\(State s _) -> s.turn) (\(State s ext) x -> State s{turn = x} ext)

_dialog :: forall pos ext. Lens' (State pos ext) (Dialog (State pos ext))
_dialog = lens (\(State s _) -> s.dialog) (\(State s ext) x -> State s{dialog = x} ext)

_levelFinished :: forall pos ext. Lens' (State pos ext) Boolean
_levelFinished = lens (\(State s _) -> s.levelFinished) (\(State s ext) x -> State s{levelFinished = x} ext)

data State pos ext = State (CoreState pos ext) ext

class Game pos ext mov | ext -> pos mov where
    play :: State pos ext -> mov -> pos
    canPlay :: State pos ext -> mov -> Boolean
    initialPosition :: State pos ext -> Random pos
    isLevelFinished :: State pos ext -> Boolean

changeTurn :: forall pos ext. State pos ext -> State pos ext
changeTurn state =
    case state ^. _mode of
        DuelMode -> _turn %~ (\x -> 1 - x) $ state
        _ -> state

undo :: forall pos ext. State pos ext -> State pos ext
undo state = fromMaybe state $ do
    let position = state ^. _position
    hs <- NArray.fromArray $ state ^. _history 
    (pure
    $ changeTurn
    $ _position .~ NArray.last hs
    $ _history .~ NArray.init hs
    $ _redoHistory %~ cons position
    $ state)

redo :: forall pos ext. State pos ext -> State pos ext
redo state = fromMaybe state $ do
    let position = state ^. _position
    hs <- NArray.fromArray $ state ^. _redoHistory
    (pure
    $ changeTurn
    $ _position .~ NArray.last hs
    $ _redoHistory .~ NArray.init hs
    $ _history %~ cons position
    $ state)

reset :: forall pos ext. State pos ext -> State pos ext
reset state = fromMaybe state $ do
    hs <- NArray.fromArray $ state ^. _history
    (pure
    $ _position .~ NArray.head hs
    $ _history .~ []
    $ _redoHistory .~ []
    $ _turn .~ 0
    $ state)

_play :: forall pos ext mov. Game pos ext mov =>
    {move :: mov, pushToHistory :: Boolean} -> State pos ext -> State pos ext
_play {move, pushToHistory} state =
    if canPlay state move then
        let position = state ^. _position
            state2 = state
                    # _position .~ play state move
                    # _history %~ cons position
                    # _redoHistory .~ [] in
        state2 # _levelFinished .~ isLevelFinished state2
    else
        state

_play' :: forall pos ext mov. Game pos ext mov =>
    mov -> State pos ext -> State pos ext
_play' move = _play {move, pushToHistory: true}

newGame :: forall pos ext mov. Game pos ext mov =>
    (State pos ext -> State pos ext)  -> State pos ext -> Random (State pos ext)
newGame f state =
    let state2 = f state in do
        position <- initialPosition state2
        let state3 = state2
                    # _position .~ position
                    # _history .~ []
                    # _redoHistory .~ []
        if null $ state2 ^. _history then
            pure state3
        else
            pure $ _dialog .~ ConfirmNewGame state3 $ state3

newGame' :: forall a pos ext mov. Game pos ext mov =>
    (a -> State pos ext -> State pos ext) -> a -> State pos ext -> Random (State pos ext)
newGame' f val = newGame $ f val

init :: forall pos ext mov. Game pos ext mov => State pos ext -> Random (State pos ext)
init = newGame identity

confirmNewGame :: forall a pos ext. State pos ext -> a -> State pos ext
confirmNewGame st _ = st # _dialog .~ NoDialog