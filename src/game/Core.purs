module Game.Core where

import MyPrelude
import Data.Array.NonEmpty (fromArray, head, init, last, toArray) as N
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Control.Alt ((<|>))
import Lib.Random (Random, runRnd, randomPick)
import Pha.Action (Action, action, randomAction, asyncAction)

data Dialog a = Rules | NoDialog | ConfirmNewGame a
data Mode = SoloMode | RandomMode | ExpertMode | DuelMode
derive instance eqMode :: Eq Mode


type PointerPosition = {left :: Number, top :: Number, width :: Number, height :: Number}

type CoreState pos ext = {
    position :: pos,
    history :: Array pos,
    redoHistory :: Array pos,
    dialog :: Dialog (GState pos ext),
    turn :: Int,
    nbRows :: Int,
    nbColumns :: Int,
    customSize :: Boolean,
    mode :: Mode,
    help :: Boolean,
    locked :: Boolean,
    showWin :: Boolean,
    pointerPosition :: Maybe PointerPosition
}

data GState pos ext = State (CoreState pos ext) ext

defaultCoreState :: forall pos ext. pos -> CoreState pos ext
defaultCoreState p = {
    position: p,
    history: [],
    redoHistory: [],
    dialog: Rules,
    turn: 0,
    nbRows: 0,
    nbColumns: 0,
    customSize: false,
    help: false,
    mode: SoloMode,
    locked: false,
    showWin: false,
    pointerPosition: Nothing
}

genState :: forall pos ext. pos -> (CoreState pos ext -> CoreState pos ext) -> ext -> GState pos ext
genState p f ext = State (f $ defaultCoreState p) ext

_core :: forall pos ext. Lens' (GState pos ext) (CoreState pos ext)
_core = lens (\(State c e) -> c) \(State _ ext) x -> State x ext

_position :: forall pos ext. Lens' (GState pos ext) pos
_position = _core ∘ lens (_.position) (_{position = _})

_history :: forall pos ext. Lens' (GState pos ext) (Array pos)
_history = _core ∘ lens (_.history) (_{history = _})

_redoHistory :: forall pos ext. Lens' (GState pos ext) (Array pos)
_redoHistory = _core ∘ lens (_.redoHistory) (_{redoHistory = _})

_mode :: forall pos ext. Lens' (GState pos ext) Mode
_mode = _core ∘ lens (_.mode) (_{mode = _})

_help :: forall pos ext. Lens' (GState pos ext) Boolean
_help = _core ∘ lens (_.help) (_{help = _})

_turn :: forall pos ext. Lens' (GState pos ext) Int
_turn = _core ∘ lens (_.turn) (_{turn = _})

_dialog :: forall pos ext. Lens' (GState pos ext) (Dialog (GState pos ext))
_dialog = _core ∘ lens (_.dialog) (_{dialog = _})

_nbRows :: forall pos ext. Lens' (GState pos ext) Int
_nbRows = _core ∘ lens (_.nbRows) (_{nbRows = _})

_nbColumns :: forall pos ext. Lens' (GState pos ext) Int
_nbColumns = _core ∘ lens (_.nbColumns) (_{nbColumns = _})

_customSize :: forall pos ext. Lens' (GState pos ext) Boolean
_customSize = _core ∘ lens (_.customSize) (_{customSize = _})

_locked :: forall pos ext. Lens' (GState pos ext) Boolean
_locked = _core ∘ lens (_.locked) (_{locked = _})

_showWin :: forall pos ext. Lens' (GState pos ext) Boolean
_showWin = _core ∘ lens (_.showWin) (_{showWin = _})

_pointerPosition :: forall pos ext. Lens' (GState pos ext) (Maybe PointerPosition)
_pointerPosition = _core ∘ lens (_.pointerPosition) (_{pointerPosition = _})

data SizeLimit = SizeLimit Int Int Int Int

class Game pos ext mov | ext -> pos mov where
    play :: GState pos ext -> mov -> pos
    canPlay :: GState pos ext -> mov -> Boolean
    initialPosition :: GState pos ext -> Random pos
    isLevelFinished :: GState pos ext -> Boolean
    sizeLimit ::  GState pos ext -> SizeLimit
    computerMove :: GState pos ext -> Maybe (Random mov)
    onNewGame :: GState pos ext -> Random (GState pos ext)

defaultSizeLimit :: forall a. a -> SizeLimit
defaultSizeLimit _ = SizeLimit 0 0 0 0

defaultOnNewGame :: forall a. a -> Random a
defaultOnNewGame = pure

changeTurn :: forall pos ext. GState pos ext -> GState pos ext
changeTurn state = if state^._mode == DuelMode then state # _turn %~ \x -> 1 - x else state

undoA :: forall pos ext. Action (GState pos ext)
undoA = action \state -> N.fromArray (state^._history) # maybe state \hs ->
    changeTurn
    $ _position .~ N.last hs
    $ _history .~ N.init hs
    $ _redoHistory %~ flip snoc (state^._position)
    $ state

redoA :: forall pos ext. Action (GState pos ext)
redoA = action \state -> N.fromArray (state^._redoHistory) # maybe state  \hs ->
    changeTurn
    $ _position .~ N.last hs
    $ _redoHistory .~ N.init hs
    $ _history %~ flip snoc (state^._position)
    $ state

resetA :: forall pos ext. Action (GState pos ext)
resetA = action \state -> N.fromArray (state^._history) # maybe state \hs ->
    _position .~ N.head hs
    $ _history .~ []
    $ _redoHistory .~ []
    $ _turn .~ 0
    $ state

toggleHelpA :: forall pos ext. Action (GState pos ext)
toggleHelpA = action (_help %~ not)

playAux :: forall pos ext mov. Game pos ext mov => mov -> GState pos ext -> GState pos ext
playAux move state =
    if canPlay state move then
        let position = state^._position in
        state # _position .~ play state move
              # _turn %~ (1 - _)
    else
        state

pushToHistory :: forall pos ext. GState pos ext -> GState pos ext
pushToHistory state = state # _history %~ flip snoc (state^._position) # _redoHistory .~ []

showVictory :: forall pos ext. Action (GState pos ext)
showVictory = asyncAction \{updateState} _ -> do
    _ <- updateState $ _showWin .~ true
    delay $ Milliseconds 1000.0
    updateState $ _showWin .~ false

computerPlay :: forall pos ext mov. Game pos ext mov => Action (GState pos ext)
computerPlay = asyncAction \{getState, updateState, dispatch} state -> do 
    computerMove state # maybe (pure state) \rndmove -> do
        move2 <- liftEffect $ runRnd rndmove
        st2 <- updateState (playAux move2)
        if isLevelFinished st2 then
            dispatch showVictory
        else
            pure st2

computerStartsA :: forall pos ext mov. Game pos ext mov => Action (GState pos ext)
computerStartsA = action pushToHistory <> computerPlay

type PlayOption = {
    showWin :: Boolean
}

playA' :: forall pos ext mov. Game pos ext mov => (PlayOption -> PlayOption) -> mov -> Action (GState pos ext)
playA' optionFn move = lockAction $ asyncAction \{getState, updateState, dispatch} state -> do
    let {showWin} = optionFn {showWin: true}
    if not $ canPlay state move then
        pure state
    else do
        st2 <- updateState (playAux move ∘ pushToHistory)
        if showWin && isLevelFinished st2 then
            dispatch(showVictory)
        else if state^._mode == ExpertMode || state^._mode == RandomMode then do
            delay $ Milliseconds 1000.0
            dispatch(computerPlay)
        else 
            pure st2

playA :: forall pos ext mov. Game pos ext mov => mov -> Action (GState pos ext)
playA = playA' identity

-- affecte à true l'attribut locked avant le début de l'action act et l'affecte à false à la fin de l'action
-- fonctionne sur toute la durée d'une action asynchrone
lockAction :: forall pos ext. Action (GState pos ext) -> Action (GState pos ext)
lockAction act = asyncAction \{getState, dispatch, updateState} state -> do
    if state^._locked then
        pure state
    else do
        _ <- updateState $ _locked .~ true
        _ <- dispatch act
        updateState $ _locked .~ false

newGameAux :: forall pos ext mov. Game pos ext mov =>
    (GState pos ext -> GState pos ext) -> (GState pos ext) -> Random (GState pos ext)
newGameAux f = \state ->
    let state2 = f state in do
        state3 <- onNewGame state2
        position <- initialPosition state3
        let state4 = state3
                    # _position .~ position
                    # _history .~ []
                    # _redoHistory .~ []
                    # _help .~ false
        
        if null (state2^._history) || isLevelFinished state then
            pure state4
        else
            pure $ _dialog .~ ConfirmNewGame state4 $ state

newGame :: forall pos ext mov. Game pos ext mov =>
    (GState pos ext -> GState pos ext) -> Action (GState pos ext)
newGame f = randomAction $ newGameAux f 

newGame' :: forall a pos ext mov. Game pos ext mov =>
    (a -> GState pos ext -> GState pos ext) -> a -> Action (GState pos ext)
newGame' f val = newGame $ f val

init :: forall pos ext mov. Game pos ext mov => GState pos ext -> Random (GState pos ext)
init = newGameAux identity

setModeA :: forall pos ext mov. Game pos ext mov => Mode -> Action (GState pos ext)
setModeA = newGame' (set _mode)

setGridSizeA :: forall pos ext mov. Game pos ext mov => Int -> Int -> Boolean -> Action (GState pos ext)
setGridSizeA nbRows nbColumns customSize = newGame $ setSize' ∘ (_customSize .~ customSize) where
    setSize' state =
        if nbRows >= minrows && nbRows <= maxrows && nbColumns >= mincols && nbColumns <= maxcols then
            state # _nbRows .~ nbRows # _nbColumns .~ nbColumns
        else
            state
        where SizeLimit minrows mincols maxrows maxcols = sizeLimit state

confirmNewGameA :: forall pos ext. GState pos ext -> Action (GState pos ext)
confirmNewGameA st = action \_ -> st # _dialog .~ NoDialog

class Game pos ext mov <= TwoPlayersGame pos ext mov | ext -> pos mov  where
    isLosingPosition :: GState pos ext -> Boolean
    possibleMoves :: GState pos ext -> Array mov

computerMove' :: forall pos ext mov. TwoPlayersGame pos ext mov => GState pos ext -> Maybe (Random mov)
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
                        moves # N.toArray # find (isLosingPosition ∘ flip playAux state)
                ) in
                    (bestMove <#> pure) <|> Just (randomPick moves)

dropA :: forall pos ext dnd. Eq dnd =>  Game pos ext {from :: dnd, to :: dnd} =>
            Lens' (GState pos ext) (Maybe dnd) -> dnd -> Action (GState pos ext)
dropA dragLens to = asyncAction \{dispatch, getState, updateState} state -> do
    case state ^. dragLens of
        Nothing -> pure state
        Just drag -> do
            _ <- updateState (dragLens .~ Nothing)
            if drag /= to then dispatch (playA { from: drag, to }) else pure state
