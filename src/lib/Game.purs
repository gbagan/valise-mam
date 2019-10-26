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
import Optic.Core (lens, Lens', set, (^.), (.~), (%~))
import Lib.Random (Random, RandomFn(..), runRnd, randomPick)
import Pha (Action(..))

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
    help :: Boolean,
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
    help: false,
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

_help :: forall pos ext. Lens' (State pos ext) Boolean
_help = lens (\(State s _) -> s.help) (\(State s ext) x -> State s{help = x} ext)

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

data SizeLimit = SizeLimit Int Int Int Int

class Game pos ext mov | ext -> pos mov where
    play :: State pos ext -> mov -> pos
    canPlay :: State pos ext -> mov -> Boolean
    initialPosition :: State pos ext -> Random pos
    isLevelFinished :: State pos ext -> Boolean
    sizeLimit ::  State pos ext -> SizeLimit
    computerMove :: State pos ext -> Maybe (Random mov)
    onNewGame :: State pos ext -> Random (State pos ext)

defaultSizeLimit :: forall a. a -> SizeLimit
defaultSizeLimit _ = SizeLimit 0 0 0 0

defaultOnNewGame :: forall a. a -> Random a
defaultOnNewGame = pure

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

toggleHelp :: forall pos ext. State pos ext -> State pos ext
toggleHelp = _help %~ not

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

computerPlay :: forall pos ext mov. Game pos ext mov => (State pos ext -> Effect Unit) -> (State pos ext) -> Aff Unit
computerPlay setState state = flip (maybe $ pure unit) (computerMove state) \rndmove -> do
    move2 <- liftEffect $ runRnd rndmove
    let st2 = _play move2 state
    liftEffect $ setState $ st2
    if isLevelFinished st2 then
        showVictory setState st2
    else
        pure unit

computerStarts :: forall pos ext mov. Game pos ext mov => Action (State pos ext)
computerStarts = Action \setState _ state -> void $ launchAff $ do
    let st2 = pushToHistory state
    computerPlay setState st2

playA :: forall pos ext mov. Game pos ext mov => mov -> Action (State pos ext)
playA move = Action \setState _ state -> void $ launchAff $ do
    if not $ canPlay state move then
        pure unit
    else do
        let st2 = _play move $ pushToHistory $ state
        liftEffect $ setState st2
        if isLevelFinished st2 then do
            showVictory setState st2
        else if state^._mode == ExpertMode then do
            delay $ Milliseconds 1000.0
            computerPlay setState st2
        else 
            pure unit


newGame :: forall pos ext mov. Game pos ext mov =>
    (State pos ext -> State pos ext) -> RandomFn (State pos ext)
newGame f = RandomFn \state ->
    let state2 = f state in do
        state3 <- onNewGame state2
        position <- initialPosition state3
        let state4 = state3
                    # _position .~ position
                    # _history .~ []
                    # _redoHistory .~ []
        
        if null $ state2 ^. _history then
            pure state4
        else
            pure $ _dialog .~ ConfirmNewGame state4 $ state

newGame' :: forall a pos ext mov. Game pos ext mov =>
    (a -> State pos ext -> State pos ext) -> a -> RandomFn (State pos ext)
newGame' f val = newGame $ f val

init :: forall pos ext mov. Game pos ext mov => State pos ext -> Random (State pos ext)
init = init' where RandomFn init' = newGame identity

setMode :: forall pos ext mov. Game pos ext mov => Mode -> RandomFn (State pos ext)
setMode = newGame' (set _mode)

setCustomSize :: forall pos ext mov. Game pos ext mov => Int -> Int -> RandomFn (State pos ext)
setCustomSize nbRows nbColumns = newGame $ setCustomSize' where
    setCustomSize' state =
        if nbRows >= minrows && nbRows <= maxrows && nbColumns >= mincols && nbColumns <= maxcols then
            state # _nbRows .~ nbRows # _nbColumns .~ nbColumns
        else
            state
        where SizeLimit minrows mincols maxrows maxcols = sizeLimit state

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
                        moves # N.toArray # find (isLosingPosition <<< flip _play state)
                ) in
                    (bestMove <#> pure) <|> Just (randomPick moves)
