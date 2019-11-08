module Game.Core where

import MyPrelude
import Data.Array.NonEmpty (fromArray, head, init, last, toArray) as N
import Lib.Random (Random, runRnd, genSeed, randomPick)
import Pha.Action (Action, action, delay, DELAY, RNG, getState, setState, setState', randomAction, randomAction') -- asyncAction)
import Effect (Effect)
import Control.Alt ((<|>))

data Dialog a = Rules | NoDialog | ConfirmNewGame a | ScoreDialog | CustomDialog
data Mode = SoloMode | RandomMode | ExpertMode | DuelMode
derive instance eqMode :: Eq Mode

--- la position d'un pointer est donné en valeur entre 0 et 1 en tant que proportion entre la coordonée et la taille de l'élément
-- où l'on enregistre les mouvements du pointeur 
type PointerPosition = {x :: Number, y :: Number}

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
    pointer :: Maybe PointerPosition
}

data GState pos ext = State (CoreState pos ext) ext

defaultCoreState :: ∀pos ext. pos -> CoreState pos ext
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
    pointer: Nothing
}

genState :: ∀pos ext. pos -> (CoreState pos ext -> CoreState pos ext) -> ext -> GState pos ext
genState p f ext = State (f $ defaultCoreState p) ext

_core :: ∀pos ext. Lens' (GState pos ext) (CoreState pos ext)
_core = lens (\(State c e) -> c) \(State _ ext) x -> State x ext

_position :: ∀pos ext. Lens' (GState pos ext) pos
_position = _core ∘ lens (_.position) (_{position = _})

_history :: ∀pos ext. Lens' (GState pos ext) (Array pos)
_history = _core ∘ lens (_.history) (_{history = _})

_redoHistory :: ∀pos ext. Lens' (GState pos ext) (Array pos)
_redoHistory = _core ∘ lens (_.redoHistory) (_{redoHistory = _})

_mode :: ∀pos ext. Lens' (GState pos ext) Mode
_mode = _core ∘ lens (_.mode) (_{mode = _})

_help :: ∀pos ext. Lens' (GState pos ext) Boolean
_help = _core ∘ lens (_.help) (_{help = _})

_turn :: ∀pos ext. Lens' (GState pos ext) Int
_turn = _core ∘ lens (_.turn) (_{turn = _})

_dialog :: ∀pos ext. Lens' (GState pos ext) (Dialog (GState pos ext))
_dialog = _core ∘ lens (_.dialog) (_{dialog = _})

_nbRows :: ∀pos ext. Lens' (GState pos ext) Int
_nbRows = _core ∘ lens (_.nbRows) (_{nbRows = _})

_nbColumns :: ∀pos ext. Lens' (GState pos ext) Int
_nbColumns = _core ∘ lens (_.nbColumns) (_{nbColumns = _})

_customSize :: ∀pos ext. Lens' (GState pos ext) Boolean
_customSize = _core ∘ lens (_.customSize) (_{customSize = _})

_locked :: ∀pos ext. Lens' (GState pos ext) Boolean
_locked = _core ∘ lens (_.locked) (_{locked = _})

_showWin :: ∀pos ext. Lens' (GState pos ext) Boolean
_showWin = _core ∘ lens (_.showWin) (_{showWin = _})

_pointer :: ∀pos ext. Lens' (GState pos ext) (Maybe PointerPosition)
_pointer = _core ∘ lens (_.pointer) (_{pointer = _})

data SizeLimit = SizeLimit Int Int Int Int

class Game pos ext mov | ext -> pos mov where
    play :: GState pos ext -> mov -> pos
    canPlay :: GState pos ext -> mov -> Boolean
    initialPosition :: GState pos ext -> Random pos
    isLevelFinished :: GState pos ext -> Boolean
    sizeLimit ::  GState pos ext -> SizeLimit
    computerMove :: GState pos ext -> Maybe (Random mov)
    onNewGame :: GState pos ext -> Random (GState pos ext)

defaultSizeLimit :: ∀a. a -> SizeLimit
defaultSizeLimit _ = SizeLimit 0 0 0 0

defaultOnNewGame :: ∀a. a -> Random a
defaultOnNewGame = pure

changeTurn :: ∀pos ext. GState pos ext -> GState pos ext
changeTurn state = if state^._mode == DuelMode then state # _turn %~ \x -> 1 - x else state

undoA :: ∀pos ext effs. Action (GState pos ext) effs
undoA = action \state -> N.fromArray (state^._history) # maybe state \hs ->
    changeTurn
    $ _position .~ N.last hs
    $ _history .~ N.init hs
    $ _redoHistory %~ flip snoc (state^._position)
    $ state

redoA :: ∀pos ext effs. Action (GState pos ext) effs
redoA = action \state -> N.fromArray (state^._redoHistory) # maybe state  \hs ->
    changeTurn
    $ _position .~ N.last hs
    $ _redoHistory .~ N.init hs
    $ _history %~ flip snoc (state^._position)
    $ state

resetA :: ∀pos ext effs. Action (GState pos ext) effs
resetA = action \state -> N.fromArray (state^._history) # maybe state \hs ->
    _position .~ N.head hs
    $ _history .~ []
    $ _redoHistory .~ []
    $ _turn .~ 0
    $ state

toggleHelpA :: ∀pos ext effs. Action (GState pos ext) effs
toggleHelpA = action (_help %~ not)

playAux :: ∀pos ext mov. Game pos ext mov => mov -> GState pos ext -> GState pos ext
playAux move state =
    if canPlay state move then
        let position = state^._position in
        state # _position .~ play state move
              # _turn %~ (1 - _)
    else
        state

pushToHistory :: ∀pos ext. GState pos ext -> GState pos ext
pushToHistory state = state # _history %~ flip snoc (state^._position) # _redoHistory .~ []

showVictory :: ∀pos ext effs. Action (GState pos ext) (delay :: DELAY | effs)
showVictory = do
    setState $ _showWin .~ true
    delay 1000
    setState $ _showWin .~ false


computerPlay :: ∀pos ext mov effs. Game pos ext mov => Action (GState pos ext) (rng :: RNG, delay :: DELAY | effs)
computerPlay = do
    state <- getState
    computerMove state # maybe (pure unit) \rndmove -> do
        st2 <- randomAction' (\st -> rndmove <#> \m -> playAux m st)
        if isLevelFinished st2 then
            showVictory
        else
            pure unit

computerStartsA :: ∀pos ext mov effs. Game pos ext mov => Action (GState pos ext) (rng :: RNG, delay :: DELAY | effs)
computerStartsA = action pushToHistory *> computerPlay

type PlayOption = {
    showWin :: Boolean
}

playA' :: ∀pos ext mov effs. Game pos ext mov => (PlayOption -> PlayOption) -> mov -> 
    Action (GState pos ext) (delay :: DELAY, rng :: RNG | effs)
playA' optionFn move = lockAction $ do 
    state <- getState
    let {showWin} = optionFn {showWin: true}
    if not $ canPlay state move then
        pure unit
    else do
        st2 <- setState' (playAux move ∘ pushToHistory)
        if showWin && isLevelFinished st2 then
            showVictory
        else if state^._mode == ExpertMode || state^._mode == RandomMode then do
            delay 1000
            computerPlay
        else 
            pure unit

playA :: ∀pos ext mov effs. Game pos ext mov => mov -> Action (GState pos ext) (delay :: DELAY, rng :: RNG | effs)
playA = playA' identity


-- affecte à true l'attribut locked avant le début de l'action act et l'affecte à false à la fin de l'action
-- fonctionne sur toute la durée d'une action asynchrone
lockAction :: ∀pos ext effs. Action (GState pos ext) effs -> Action (GState pos ext) effs 
lockAction act = do
    state <- getState
    if state^._locked then
        pure unit
    else do
        setState $ _locked .~ true
        act
        setState $ _locked .~ false

newGameAux :: ∀pos ext mov. Game pos ext mov =>
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

newGame :: ∀pos ext mov effs. Game pos ext mov =>
    (GState pos ext -> GState pos ext) -> Action (GState pos ext) (rng :: RNG | effs)
newGame f = randomAction $ newGameAux f

newGame' :: ∀a pos ext mov effs. Game pos ext mov =>
    (a -> GState pos ext -> GState pos ext) -> a -> Action (GState pos ext) (rng :: RNG | effs)
newGame' f val = newGame $ f val

init :: ∀pos ext mov. Game pos ext mov => GState pos ext -> Effect (GState pos ext)
init st = genSeed <#> \seed -> runRnd seed $ newGameAux identity st


setModeA :: ∀pos ext mov effs. Game pos ext mov => Mode -> Action (GState pos ext) (rng :: RNG | effs)
setModeA = newGame' (set _mode)

setGridSizeA :: ∀pos ext mov effs. Game pos ext mov => Int -> Int -> Boolean -> Action (GState pos ext) (rng :: RNG | effs)
setGridSizeA nbRows nbColumns customSize = newGame $ setSize' ∘ (_customSize .~ customSize) where
    setSize' state =
        if nbRows >= minrows && nbRows <= maxrows && nbColumns >= mincols && nbColumns <= maxcols then
            state # _nbRows .~ nbRows # _nbColumns .~ nbColumns
        else
            state
        where SizeLimit minrows mincols maxrows maxcols = sizeLimit state

confirmNewGameA :: ∀pos ext effs. GState pos ext -> Action (GState pos ext) effs
confirmNewGameA st = action \_ -> st # _dialog .~ NoDialog

class Game pos ext mov <= TwoPlayersGame pos ext mov | ext -> pos mov  where
    isLosingPosition :: GState pos ext -> Boolean
    possibleMoves :: GState pos ext -> Array mov


computerMove' :: ∀pos ext mov. TwoPlayersGame pos ext mov => GState pos ext -> Maybe (Random mov)
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
                
dropA :: ∀pos ext dnd effs. Eq dnd =>  Game pos ext {from :: dnd, to :: dnd} =>
            Lens' (GState pos ext) (Maybe dnd) -> dnd -> Action (GState pos ext) (rng :: RNG, delay :: DELAY | effs)
dropA dragLens to = do
    state <- getState
    case state ^. dragLens of
        Nothing -> pure unit
        Just drag -> do
            setState (dragLens .~ Nothing)
            if drag /= to then playA { from: drag, to } else pure unit
