module Game.Core where
import MyPrelude
import Data.List (List(..))
import Data.List (last, null) as L
import Data.Map (Map, empty) as M
import Pha.Effects.Random (RNG, sample)
import Run (Run)
import Pha.Action (Action, getState, setState) 
import Pha.Effects.Delay (delay, DELAY)
import Game.Effs (EFFS)

-- ConfirmNewGame contient le futur state si l'utilisateur valide de commencer une nouvelle partie
data Dialog a = Rules | NoDialog | ConfirmNewGameDialog a | ScoreDialog | CustomDialog

data Mode = SoloMode | RandomMode | ExpertMode | DuelMode
derive instance eqMode :: Eq Mode

data Turn = Turn1 | Turn2
derive instance eqTurn :: Eq Turn

type PointerPosition = {x :: Number, y :: Number}

type CoreState pos ext = {
    position :: pos,
    history :: List pos,
    redoHistory :: List pos,
    dialog :: Dialog (GState pos ext),
    turn :: Turn,
    nbRows :: Int,
    nbColumns :: Int,
    customSize :: Boolean,
    mode :: Mode,  --- mode pour les jeux à deux joueurs
    help :: Boolean, --- si l'aide est activée ou non
    locked :: Boolean,  ---- quand locked est à true, aucune action de l'utiliateur n'est possible
    showWin :: Boolean,
    scores :: M.Map String (Tuple Int pos),
    pointer :: Maybe PointerPosition --- position du pointeur en % relativement au plateau de jeu
}

data GState pos ext = State (CoreState pos ext) ext

defaultCoreState :: ∀pos ext. pos -> CoreState pos ext
defaultCoreState p = {
    position: p,
    history: Nil,
    redoHistory: Nil,
    dialog: Rules,
    turn: Turn1,
    nbRows: 0,
    nbColumns: 0,
    customSize: false,
    help: false,
    mode: SoloMode,
    locked: false,
    showWin: false,
    scores: M.empty,
    pointer: Nothing
}

-- fonction pour faciliter la création d'un état initial
genState :: ∀pos ext. pos -> (CoreState pos ext -> CoreState pos ext) -> ext -> GState pos ext
genState p f ext = State (f $ defaultCoreState p) ext

_core :: ∀pos ext. Lens' (GState pos ext) (CoreState pos ext)
_core = lens (\(State c e) -> c) \(State _ e) c -> State c e

_ext :: ∀pos ext. Lens' (GState pos ext) ext
_ext = lens (\(State c e) -> e) \(State c _) e -> State c e

_position :: ∀pos ext. Lens' (GState pos ext) pos
_position = _core ∘ lens _.position _{position = _}

_history :: ∀pos ext. Lens' (GState pos ext) (List pos)
_history = _core ∘ lens (_.history) _{history = _}

_redoHistory :: ∀pos ext. Lens' (GState pos ext) (List pos)
_redoHistory = _core ∘ lens _.redoHistory _{redoHistory = _}

_mode :: ∀pos ext. Lens' (GState pos ext) Mode
_mode = _core ∘ lens _.mode _{mode = _}

_help :: ∀pos ext. Lens' (GState pos ext) Boolean
_help = _core ∘ lens _.help _{help = _}

_turn :: ∀pos ext. Lens' (GState pos ext) Turn
_turn = _core ∘ lens _.turn _{turn = _}

_dialog :: ∀pos ext. Lens' (GState pos ext) (Dialog (GState pos ext))
_dialog = _core ∘ lens _.dialog _{dialog = _}

_nbRows :: ∀pos ext. Lens' (GState pos ext) Int
_nbRows = _core ∘ lens _.nbRows _{nbRows = _}

_nbColumns :: ∀pos ext. Lens' (GState pos ext) Int
_nbColumns = _core ∘ lens _.nbColumns _{nbColumns = _}

_customSize :: ∀pos ext. Lens' (GState pos ext) Boolean
_customSize = _core ∘ lens _.customSize _{customSize = _}

_locked :: ∀pos ext. Lens' (GState pos ext) Boolean
_locked = _core ∘ lens _.locked _{locked = _}

_showWin :: ∀pos ext. Lens' (GState pos ext) Boolean
_showWin = _core ∘ lens _.showWin _{showWin = _}

_pointer :: ∀pos ext. Lens' (GState pos ext) (Maybe PointerPosition)
_pointer = _core ∘ lens _.pointer _{pointer = _}

_scores :: ∀pos ext. Lens' (GState pos ext) (M.Map String (Tuple Int pos))
_scores = _core ∘ lens  _.scores _{scores = _}

data SizeLimit = SizeLimit Int Int Int Int

class Game pos ext mov | ext -> pos mov where
    play :: GState pos ext -> mov -> Maybe pos
    initialPosition :: ∀r. GState pos ext -> Run (rng :: RNG | r) pos
    isLevelFinished :: GState pos ext -> Boolean
    sizeLimit ::  GState pos ext -> SizeLimit
    computerMove :: ∀r. GState pos ext -> Run (rng :: RNG | r) (Maybe mov)
    onNewGame :: ∀r. GState pos ext -> Run (rng :: RNG | r) (GState pos ext)
    updateScore :: GState pos ext -> Tuple (GState pos ext) Boolean

canPlay :: ∀pos ext mov. Game pos ext mov => GState pos ext -> mov -> Boolean
canPlay st mov = isJust (play st mov)

defaultSizeLimit :: ∀a. a -> SizeLimit
defaultSizeLimit _ = SizeLimit 0 0 0 0

defaultOnNewGame :: ∀a r. a -> Run (rng :: RNG | r) a
defaultOnNewGame = pure

oppositeTurn :: Turn -> Turn
oppositeTurn Turn1 = Turn2
oppositeTurn _ = Turn1

changeTurn :: ∀pos ext. GState pos ext -> GState pos ext
changeTurn state = state # _turn %~ \x -> if state^._mode == DuelMode then oppositeTurn x else Turn1

data CoreMsg = 
      Undo
    | Redo 
    | Reset 
    | ToggleHelp
    | SetMode Mode 
    | SetGridSize Int Int Boolean
    | SetCustomSize Boolean 
    | SetNoDialog 
    | SetRulesDialog
    | SetScoreDialog
    | ConfirmNewGame
    | SetPointer (Maybe { x :: Number, y :: Number })
    | ComputerStarts
    | Init

class MsgWithCore a where
    core :: CoreMsg -> a

coreUpdate :: ∀pos ext mov. Game pos ext mov => CoreMsg -> Action (GState pos ext) EFFS
coreUpdate Undo = setState \state -> case state^._history of
    Nil -> state
    Cons h rest ->
        state # changeTurn
              # _position .~ h
              # _history .~ rest
              # _redoHistory %~ Cons (state^._position)

coreUpdate Redo = setState \state -> case state^._redoHistory of
    Nil -> state
    Cons h rest ->
        state # changeTurn
              # _position .~ h
              # _redoHistory .~ rest
              # _history %~ Cons (state^._position)

coreUpdate Reset = setState \state -> case L.last (state^._history) of
    Nothing -> state
    Just x -> state # _position .~ x
                    # _history .~ Nil
                    # _redoHistory .~ Nil
                    # _turn .~ Turn1

coreUpdate ToggleHelp =  setState (_help %~ not)
coreUpdate (SetMode mode) = newGame (_mode .~ mode)
coreUpdate (SetGridSize nbRows nbColumns customSize) = 
    newGame $ setSize' ∘ (_customSize .~ customSize) where
    setSize' state =
        if nbRows >= minrows && nbRows <= maxrows && nbColumns >= mincols && nbColumns <= maxcols then
            state # _nbRows .~ nbRows # _nbColumns .~ nbColumns
        else
            state
        where SizeLimit minrows mincols maxrows maxcols = sizeLimit state
coreUpdate (SetCustomSize b) = setState $ _customSize .~ true
coreUpdate SetNoDialog = setState $ _dialog .~ NoDialog
coreUpdate SetRulesDialog = setState $ _dialog .~ Rules
coreUpdate SetScoreDialog = setState $ _dialog .~ ScoreDialog
coreUpdate ConfirmNewGame = setState $ \state ->
                        case state^._dialog of
                            ConfirmNewGameDialog st -> st
                            _ -> state
coreUpdate (SetPointer pos) = setState $ _pointer .~ pos
coreUpdate ComputerStarts = setState (pushToHistory ∘ (_turn %~ oppositeTurn)) *> computerPlay
coreUpdate Init = newGame identity

playAux :: ∀pos ext mov. Game pos ext mov => mov -> GState pos ext -> Maybe (GState pos ext)
playAux move state =
    play state move <#> \pos ->
        state # _position .~ pos
              # _turn %~ oppositeTurn

-- met dans l'historique la position actuelle
pushToHistory :: ∀pos ext. GState pos ext -> GState pos ext
pushToHistory state = state # _history %~ Cons (state^._position) # _redoHistory .~ Nil

showVictory :: ∀pos ext effs. Action (GState pos ext) (delay :: DELAY | effs)
showVictory = do
    setState $ _showWin .~ true
    delay 1000
    setState $ _showWin .~ false

computerPlay :: ∀pos ext mov effs. Game pos ext mov => Action (GState pos ext) (rng :: RNG, delay :: DELAY | effs)
computerPlay = do
    state <- getState
    move <- computerMove state
    case flip playAux state =<< move of
        Nothing -> pure unit
        Just st2 -> do
            setState (const st2)
            when (isLevelFinished st2) showVictory

playA :: ∀pos ext mov effs. Game pos ext mov => mov -> Action (GState pos ext) (delay :: DELAY, rng :: RNG | effs)
playA move = lockAction $ do
    state <- getState
    case playAux move $ pushToHistory $ state of
        Nothing -> pure unit
        Just st2 -> do
            setState \_ -> st2
            if isLevelFinished st2 then do
                let st3 ∧ isNewRecord = updateScore st2
                setState \_ -> st3
                when isNewRecord showVictory
            else if st2^._mode == ExpertMode || st2^._mode == RandomMode then
                delay 1000 *> computerPlay
            else 
                pure unit
-- affecte à true l'attribut locked avant le début de l'action act et l'affecte à false à la fin de l'action
-- fonctionne sur toute la durée d'une action asynchrone
-- ne fait rien si locked est déjà à true
lockAction :: ∀pos ext effs. Action (GState pos ext) effs -> Action (GState pos ext) effs 
lockAction act = unlessM (view _locked <$> getState) do
        setState (_locked .~ true)
        act
        setState (_locked .~ false)

newGame :: ∀pos ext mov effs. Game pos ext mov =>
    (GState pos ext -> GState pos ext) -> Action (GState pos ext) (rng :: RNG | effs)
newGame f = do
    state <- getState
    let state2 = f state
    state3 <- onNewGame state2
    position <- initialPosition state3
    let state4 = state3
                # _position .~ position
                # _history .~ Nil
                # _redoHistory .~ Nil
                # _help .~ false
                # _scores ∘ at "custom" .~ Nothing
        
    setState $
        if L.null (state2^._history) || isLevelFinished state then
            const state4
        else
            _dialog .~ ConfirmNewGameDialog state4

class Game pos ext mov <= TwoPlayersGame pos ext mov | ext -> pos mov  where
    isLosingPosition :: GState pos ext -> Boolean
    possibleMoves :: GState pos ext -> Array mov

computerMove' :: ∀pos ext mov r. TwoPlayersGame pos ext mov => GState pos ext -> Run (rng :: RNG | r) (Maybe mov)
computerMove' state =
    if isLevelFinished state then
        pure Nothing
    else
        let moves = possibleMoves state in
        let bestMove = (
            if state^._mode == RandomMode then
                Nothing
            else
                moves # find (maybe false isLosingPosition ∘ flip playAux state)
        ) in
            case bestMove of
                Just _ -> pure bestMove
                Nothing -> sample moves 

data Objective = Minimize | Maximize
derive instance eqObjective :: Eq Objective 
data ShowWinStrategy = AlwaysShowWin | NeverShowWin | ShowWinOnNewRecord
derive instance eqSws :: Eq ShowWinStrategy 

class Game pos ext mov <= ScoreGame pos ext mov | ext -> pos mov  where
    objective :: GState pos ext -> Objective
    scoreFn :: GState pos ext -> Int
    scoreHash ::  GState pos ext -> String
    isCustomGame :: GState pos ext -> Boolean                    

scoreHash' :: ∀pos ext mov. ScoreGame pos ext mov => GState pos ext -> String
scoreHash' state = if isCustomGame state then "custom" else scoreHash state

updateScore' :: ∀pos ext mov. ScoreGame pos ext mov => ShowWinStrategy -> GState pos ext -> Tuple (GState pos ext) Boolean
updateScore' strat state =
    let score = scoreFn state
        hash = scoreHash' state 
        cmp = if objective state == Minimize then (<) else (>)
        oldScore = bestScore state
        isNewRecord = maybe true (cmp score ∘ fst) oldScore
        isNewRecord' = isNewRecord && strat == ShowWinOnNewRecord || strat == AlwaysShowWin
        st2 = state # _scores ∘ at hash %~ if isNewRecord then \_ -> Just (score ∧ (state^._position)) else identity
    in st2 ∧ isNewRecord'

bestScore :: ∀pos ext mov. ScoreGame pos ext mov => GState pos ext -> Maybe (Tuple Int pos)
bestScore state = state ^. (_scores ∘ at (scoreHash' state))

data DndMsg i = Drag i | Drop i | Leave | DropOnBoard

class MsgWithDnd msg i | msg -> i where
    dndmsg :: DndMsg i -> msg

dndUpdate :: ∀pos ext i. Eq i => Game pos ext {from :: i, to :: i} => 
    Lens' (GState pos ext) (Maybe i) -> DndMsg i -> Action (GState pos ext) EFFS
dndUpdate _dragged (Drag i) = setState $ _dragged .~ Just i
dndUpdate _dragged (Drop i) = dropA _dragged i
dndUpdate _dragged Leave = setState $ _dragged .~ Nothing
dndUpdate _dragged DropOnBoard = setState $ _dragged .~ Nothing

dropA :: ∀pos ext dnd effs. Eq dnd =>  Game pos ext {from :: dnd, to :: dnd} =>
            Lens' (GState pos ext) (Maybe dnd) -> dnd -> Action (GState pos ext) (rng :: RNG, delay :: DELAY | effs)
dropA dragLens to = do
    state <- getState
    case state ^. dragLens of
        Nothing -> pure unit
        Just drag -> do
            setState (dragLens .~ Nothing)
            if drag /= to then playA { from: drag, to } else pure unit
