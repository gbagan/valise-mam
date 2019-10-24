module Lib.Game where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Array (snoc, null, find)
import Data.Array.NonEmpty (fromArray, head, init, last, toArray) as N
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff)
import Effect.Class (liftEffect)
import Control.Alt ((<|>))
import Optic.Core (lens, Lens', (^.), (.~), (%~))
import Lib.Random (Random, runRnd, randomPick)
import Pha (Action, action, rndAction, lensAction)

data Dialog a = Rules | NoDialog | ConfirmNewGame a
data Mode = SoloMode | RandomMode | ExpertMode | DuelMode
derive instance eqMode :: Eq Mode

type CoreState pos ext = {
    position :: pos,
    history :: Array pos,
    redoHistory :: Array pos,
    levelFinished :: Boolean,
    dialog :: Dialog (State pos ext),
    turn :: Int,
    nbRows :: Int,
    nbColumns :: Int,
    mode :: Mode,
    showWin :: Boolean
}

data State pos ext = State (CoreState pos ext) ext

defaultCoreState :: forall pos ext. pos -> CoreState pos ext
defaultCoreState p = {
    position: p,
    history: [],
    redoHistory: [],
    levelFinished: false,
    dialog: Rules,
    turn: 0,
    nbRows: 0,
    nbColumns: 0,
    mode: SoloMode,
    showWin: false
}

genState :: forall pos ext. pos -> (CoreState pos ext -> CoreState pos ext) -> ext -> State pos ext
genState p f ext = State (f $ defaultCoreState p) ext

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

_nbRows :: forall pos ext. Lens' (State pos ext) Int
_nbRows = lens (\(State s _) -> s.nbRows) (\(State s ext) x -> State s{nbRows = x} ext)

_nbColumns :: forall pos ext. Lens' (State pos ext) Int
_nbColumns = lens (\(State s _) -> s.nbColumns) (\(State s ext) x -> State s{nbColumns = x} ext)

_levelFinished :: forall pos ext. Lens' (State pos ext) Boolean
_levelFinished = lens (\(State s _) -> s.levelFinished) (\(State s ext) x -> State s{levelFinished = x} ext)

_showWin :: forall pos ext. Lens' (State pos ext) Boolean
_showWin = lens (\(State s _) -> s.showWin) (\(State s ext) x -> State s{showWin = x} ext)

class LensAction a b c | c -> b where 
    _lensaction :: Lens' a b -> c -> Action a
      
instance lensaction1 :: LensAction  a (State pos aux) (State pos aux -> State pos aux) where
    _lensaction lens f = lensAction lens $ action f

instance lensactionrnd :: LensAction  a (State pos aux) (State pos aux -> Random(State pos aux)) where
    _lensaction lens f = lensAction lens $ rndAction f

infixl 3  _lensaction as 🎲

class Game pos ext mov | ext -> pos mov where
    play :: State pos ext -> mov -> pos
    canPlay :: State pos ext -> mov -> Boolean
    initialPosition :: State pos ext -> Random pos
    isLevelFinished :: State pos ext -> Boolean
    computerMove :: State pos ext -> Maybe (Random mov)

changeTurn :: forall pos ext. State pos ext -> State pos ext
changeTurn state = if state^._mode == DuelMode then state # _turn %~ \x -> 1 - x else state

undo :: forall pos ext. State pos ext -> State pos ext
undo state = flip (maybe state) (N.fromArray $ state^._history) \hs ->
    changeTurn
    $ _position .~ N.last hs
    $ _history .~ N.init hs
    $ _redoHistory %~ flip snoc (state^._position)
    $ state

redo :: forall pos ext. State pos ext -> State pos ext
redo state = flip (maybe state) (N.fromArray $ state^._redoHistory) \hs ->
    changeTurn
    $ _position .~ N.last hs
    $ _redoHistory .~ N.init hs
    $ _history %~ flip snoc (state^._position)
    $ state

reset :: forall pos ext. State pos ext -> State pos ext
reset state = flip (maybe state) (N.fromArray $ state^._history) \hs ->
    _position .~ N.head hs
    $ _history .~ []
    $ _redoHistory .~ []
    $ _turn .~ 0
    $ state

_play :: forall pos ext mov. Game pos ext mov => mov -> State pos ext -> State pos ext
_play move state =
    if canPlay state move then
        let position = state^._position
            state2 = state
                    # _position .~ play state move
                    # _turn %~ (1 - _) in
        state2 # _levelFinished .~ isLevelFinished state2
    else
        state

pushToHistory :: forall pos ext. State pos ext -> State pos ext
pushToHistory state = state # _history %~ flip snoc (state^._position) # _redoHistory .~ []

showVictory :: forall pos ext. (State pos ext -> Effect Unit) -> State pos ext -> Aff Unit
showVictory setState state = do
    liftEffect $ setState $ _showWin .~ true $ state
    delay $ Milliseconds 1000.0
    liftEffect $ setState $ state
    pure unit

_play' :: forall pos ext mov. Game pos ext mov => mov -> Action (State pos ext)
_play' move setState state = void $ launchAff $ do
    if not $ canPlay state move then
        pure unit
    else do
        let st2 = _play move $ pushToHistory $ state
        liftEffect $ setState st2
        if isLevelFinished st2 then do
            showVictory setState st2
        else if state^._mode == ExpertMode then do
            delay $ Milliseconds 1000.0
            flip (maybe $ pure unit) (computerMove st2) \rndmove -> do
                move2 <- liftEffect $ runRnd rndmove
                let st3 = _play move2 st2
                liftEffect $ setState $ st3
                if isLevelFinished st3 then
                    showVictory setState st3
                else
                    pure unit
            
        else 
            pure unit


newGame :: forall pos ext mov. Game pos ext mov =>
    (State pos ext -> State pos ext) -> State pos ext -> Random (State pos ext)
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
            pure $ _dialog .~ ConfirmNewGame state3 $ state

newGame' :: forall a pos ext mov. Game pos ext mov =>
    (a -> State pos ext -> State pos ext) -> a -> State pos ext -> Random (State pos ext)
newGame' f val = newGame $ f val

init :: forall pos ext mov. Game pos ext mov => State pos ext -> Random (State pos ext)
init = newGame identity

confirmNewGame :: forall pos ext. State pos ext -> State pos ext -> State pos ext
confirmNewGame st _ = st # _dialog .~ NoDialog

class Game pos ext mov <= TwoPlayersGame pos ext mov | ext -> pos mov  where
    isLosingPosition :: State pos ext -> Boolean
    possibleMoves :: State pos ext -> Array mov

computerMove' :: forall pos ext mov. TwoPlayersGame pos ext mov => State pos ext -> Maybe (Random mov)
computerMove' state = 
    if isLevelFinished state then
        Nothing
    else
        N.fromArray (possibleMoves state) >>=
            \moves ->
                let bestMove = (
                    if state^._mode == RandomMode then
                        Nothing
                    else
                        moves # N.toArray # find (\move -> isLosingPosition $ _play move state)
                ) in
                    (bestMove <#> pure) <|> Just (randomPick moves)
