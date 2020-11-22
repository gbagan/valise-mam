module Game.Core where

import MyPrelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.List (List(..))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Lib.Random (Random)
import Lib.Random as R
import Lib.Update (Update, get, modify, put, delay, randomEval, randomly, storageGet, storagePut)

-- ConfirmNewGame contient le futur état d'une nouvelle partie
data Dialog a = Rules | NoDialog | ConfirmNewGameDialog a | ScoreDialog | CustomDialog

data Mode = SoloMode | RandomMode | ExpertMode | DuelMode
derive instance eqMode ∷ Eq Mode

data Turn = Turn1 | Turn2
derive instance eqTurn ∷ Eq Turn

type PointerPosition = {x ∷ Number, y ∷ Number}

type CoreState pos ext = {
    position ∷ pos,
    history ∷ List pos,
    redoHistory ∷ List pos,
    dialog ∷ Dialog (GState pos ext),
    turn ∷ Turn,
    nbRows ∷ Int,
    nbColumns ∷ Int,
    customSize ∷ Boolean,
    mode ∷ Mode,  --- mode pour les jeux à deux joueurs
    help ∷ Boolean, --- si l'aide est activée ou non
    locked ∷ Boolean,  ---- quand locked est à true, aucune action de l'utiliateur n'est possible
    showWin ∷ Boolean,
    scores ∷ Map String (Tuple Int pos),
    pointer ∷ Maybe PointerPosition --- position du pointeur en % relativement au plateau de jeu
}

data GState pos ext = State (CoreState pos ext) ext

defaultCoreState ∷ ∀pos ext. pos → CoreState pos ext
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
genState ∷ ∀pos ext. pos → (CoreState pos ext → CoreState pos ext) → ext → GState pos ext
genState p f ext = State (f $ defaultCoreState p) ext

-- lenses 
_core ∷ ∀pos ext. Lens' (GState pos ext) (CoreState pos ext)
_core = lens (\(State c e) → c) \(State _ e) c → State c e

_ext ∷ ∀pos ext. Lens' (GState pos ext) ext
_ext = lens (\(State c e) → e) \(State c _) e → State c e

_position ∷ ∀pos ext. Lens' (GState pos ext) pos
_position = _core ∘ lens _.position _{position = _}

_history ∷ ∀pos ext. Lens' (GState pos ext) (List pos)
_history = _core ∘ lens (_.history) _{history = _}

_redoHistory ∷ ∀pos ext. Lens' (GState pos ext) (List pos)
_redoHistory = _core ∘ lens _.redoHistory _{redoHistory = _}

_mode ∷ ∀pos ext. Lens' (GState pos ext) Mode
_mode = _core ∘ lens _.mode _{mode = _}

_help ∷ ∀pos ext. Lens' (GState pos ext) Boolean
_help = _core ∘ lens _.help _{help = _}

_turn ∷ ∀pos ext. Lens' (GState pos ext) Turn
_turn = _core ∘ lens _.turn _{turn = _}

_dialog ∷ ∀pos ext. Lens' (GState pos ext) (Dialog (GState pos ext))
_dialog = _core ∘ lens _.dialog _{dialog = _}

_nbRows ∷ ∀pos ext. Lens' (GState pos ext) Int
_nbRows = _core ∘ lens _.nbRows _{nbRows = _}

_nbColumns ∷ ∀pos ext. Lens' (GState pos ext) Int
_nbColumns = _core ∘ lens _.nbColumns _{nbColumns = _}

_customSize ∷ ∀pos ext. Lens' (GState pos ext) Boolean
_customSize = _core ∘ lens _.customSize _{customSize = _}

_locked ∷ ∀pos ext. Lens' (GState pos ext) Boolean
_locked = _core ∘ lens _.locked _{locked = _}

_showWin ∷ ∀pos ext. Lens' (GState pos ext) Boolean
_showWin = _core ∘ lens _.showWin _{showWin = _}

_pointer ∷ ∀pos ext. Lens' (GState pos ext) (Maybe PointerPosition)
_pointer = _core ∘ lens _.pointer _{pointer = _}

_scores ∷ ∀pos ext. Lens' (GState pos ext) (Map String (Tuple Int pos))
_scores = _core ∘ lens  _.scores _{scores = _}

data SizeLimit = SizeLimit Int Int Int Int

class Game pos ext mov | ext → pos mov where
    name ∷ GState pos ext → String
    play ∷ GState pos ext → mov → Maybe pos
    initialPosition ∷ GState pos ext → Random pos
    isLevelFinished ∷ GState pos ext → Boolean
    sizeLimit ∷ GState pos ext → SizeLimit
    computerMove ∷ GState pos ext → Random (Maybe mov)
    onNewGame ∷ GState pos ext → Random (GState pos ext)
    onPositionChange ∷ GState pos ext → GState pos ext
    updateScore ∷ GState pos ext → Tuple (GState pos ext) Boolean
    saveToJson ∷ GState pos ext → Maybe Json
    loadFromJson ∷ GState pos ext → Json → GState pos ext

canPlay ∷ ∀pos ext mov. Game pos ext mov ⇒ GState pos ext → mov → Boolean
canPlay st mov = isJust (play st mov)

defaultSizeLimit ∷ ∀a. a → SizeLimit
defaultSizeLimit _ = SizeLimit 0 0 0 0

defaultOnNewGame ∷ ∀a. a → Random a
defaultOnNewGame = pure

oppositeTurn ∷ Turn → Turn
oppositeTurn Turn1 = Turn2
oppositeTurn _ = Turn1

changeTurn ∷ ∀pos ext. GState pos ext → GState pos ext
changeTurn state = state # _turn %~ \x → if state^._mode == DuelMode then oppositeTurn x else Turn1

data CoreMsg = 
      Undo
    | Redo 
    | Reset
    | Clear
    | ToggleHelp
    | SetMode Mode 
    | SetGridSize Int Int Boolean
    | SetCustomSize Boolean 
    | SetNoDialog 
    | SetRulesDialog
    | SetScoreDialog
    | ConfirmNewGame
    | SetPointer (Maybe { x ∷ Number, y ∷ Number })
    | ComputerStarts
    | Init

class MsgWithCore a where
    core ∷ CoreMsg → a

coreUpdate ∷ ∀pos ext mov. Game pos ext mov ⇒ CoreMsg → Update (GState pos ext)
coreUpdate Undo = modify \state → case state^._history of
    Nil → state
    Cons h rest →
        state # changeTurn
              # _position .~ h
              # _history .~ rest
              # _redoHistory %~ Cons (state^._position)
              # onPositionChange

coreUpdate Redo = modify \state → case state^._redoHistory of
    Nil → state
    Cons h rest →
        state # changeTurn
              # _position .~ h
              # _redoHistory .~ rest
              # _history %~ Cons (state^._position)
              # onPositionChange

coreUpdate Reset = modify \state → case L.last (state^._history) of
    Nothing → state
    Just x → state # _position .~ x
                    # _history .~ Nil
                    # _redoHistory .~ Nil
                    # _turn .~ Turn1
                    # onPositionChange
coreUpdate Clear = newGame identity
coreUpdate ToggleHelp = modify $ _help %~ not
coreUpdate (SetMode mode) = newGame (_mode .~ mode)
coreUpdate (SetGridSize nbRows nbColumns customSize) = 
    newGame $ setSize' ∘ (_customSize .~ customSize) where
    setSize' state =
        if nbRows >= minrows && nbRows <= maxrows && nbColumns >= mincols && nbColumns <= maxcols then
            state # _nbRows .~ nbRows # _nbColumns .~ nbColumns
        else
            state
        where SizeLimit minrows mincols maxrows maxcols = sizeLimit state
coreUpdate (SetCustomSize bool) = modify $ _customSize .~ bool
coreUpdate SetNoDialog = modify $ _dialog .~ NoDialog
coreUpdate SetRulesDialog = modify $ _dialog .~ Rules
coreUpdate SetScoreDialog = modify $ _dialog .~ ScoreDialog
coreUpdate ConfirmNewGame = modify \state →
                        case state^._dialog of
                            ConfirmNewGameDialog st → st
                            _ → state
coreUpdate (SetPointer pos) = modify $ _pointer .~ pos
coreUpdate ComputerStarts = do
    modify $ pushToHistory >>> (_turn %~ oppositeTurn)
    computerPlay
coreUpdate Init = do
    newGame identity
    st ← get
    val ← storageGet ("valise-" <> name st)
    case val of
        Nothing → pure unit
        Just text →
            case jsonParser text of
                Left _ → pure unit
                Right json → do
                    put $ loadFromJson st json

playAux ∷ ∀pos ext mov. Game pos ext mov ⇒ mov → GState pos ext → Maybe (GState pos ext)
playAux move state =
    play state move <#> \pos →
        state # _position .~ pos
              # _turn %~ oppositeTurn
              # onPositionChange

-- met dans l'historique la position actuelle
pushToHistory ∷ ∀pos ext. GState pos ext → GState pos ext
pushToHistory state = state # _history %~ Cons (state^._position) # _redoHistory .~ Nil

showVictory ∷ ∀pos ext. Update (GState pos ext)
showVictory = do
    modify $ _showWin .~ true
    delay 1000
    modify $ _showWin .~ false

computerPlay ∷ ∀pos ext mov. Game pos ext mov ⇒ Update (GState pos ext)
computerPlay = do
    state ← get
    move ← randomEval $ computerMove state
    case flip playAux state =<< move of
        Nothing → pure unit
        Just st2 → do
            put st2
            when (isLevelFinished st2) showVictory

saveToStorage ∷ ∀pos ext mov. Game pos ext mov ⇒ Update (GState pos ext)
saveToStorage = do
    state ← get
    case saveToJson state of
        Nothing → pure unit
        Just json → storagePut ("valise-" <> name state) (stringify json)

playA ∷ ∀pos ext mov. Game pos ext mov ⇒ mov → Update (GState pos ext)
playA move = lockAction $ do
    state ← get
    case playAux move $ pushToHistory $ state of
        Nothing → pure unit
        Just st2 → do
            put st2
            if isLevelFinished st2 then do
                let st3 ∧ isNewRecord = updateScore st2
                put st3
                when isNewRecord do
                    saveToStorage   -- todo: ne va pas fonctionner pour 8 reines
                    showVictory
            else if st2^._mode == ExpertMode || st2^._mode == RandomMode then
                delay 1000 *> computerPlay
            else 
                pure unit

-- | Empêche d'autres actions d'être effectués durant la durée de l'action
-- | en mettant locked à true au début de l'action et à false à la fin de l'action.
-- | L'action n'est pas executé si locked est déjà à true
lockAction ∷ ∀pos ext. Update (GState pos ext) → Update (GState pos ext)
lockAction action = unlessM (view _locked <$> get) do
        modify (_locked .~ true)
        action
        modify (_locked .~ false)

-- | fonction auxiliaire pour newGame
newGameAux ∷ ∀pos ext mov. Game pos ext mov ⇒
        (GState pos ext → GState pos ext) → GState pos ext → Random (GState pos ext)
newGameAux f state = do 
    let state2 = f state
    state3 ← onNewGame state2
    position ← initialPosition state3
    pure $ state3
        # _position .~ position
        # _history .~ Nil
        # _redoHistory .~ Nil
        # _help .~ false
        # _turn .~ Turn1
        # _scores ∘ at "custom" .~ Nothing


-- | créé une nouvelle partie en applicant la fonction f donnée en argument.
-- | Par défault, ouvre une boite de dialogue pour confirmer la nouvelle partie
-- | L'état qu'il y aura après une nouvelle partie est stockée dans ConfirmNewGameDialog s
-- | Si l'utilisateur confirme, on remplace l'état courant par s
-- | Si l'historique est vide ou si la partie est finie, on lance une nouvelle partie
-- | sans demander confirmation à l'utilisateur
newGame ∷ ∀pos ext mov. Game pos ext mov ⇒
    (GState pos ext → GState pos ext) → Update (GState pos ext)
newGame f = randomly \state →
    let rstate = newGameAux f state in
    if L.null (state^._history) || isLevelFinished state then
        rstate
    else
        rstate <#> \s → state # _dialog .~ ConfirmNewGameDialog s

-- | classe facilitant l'implétentation d'une IA pour les jeux à deux joueurs
-- | nécessite de pouvoir calculer efficacement l'ensemble des coups légaux pour un joueur
-- | et déterminer si une position est perdante ou non
class Game pos ext mov <= TwoPlayersGame pos ext mov | ext → pos mov  where
    isLosingPosition ∷ GState pos ext → Boolean
    possibleMoves ∷ GState pos ext → Array mov

-- | implémentation de la fonction computerMove de la classe Game
-- | nécessite l'implémentation de la classe TwoPlayersGame
computerMove' ∷ ∀pos ext mov. TwoPlayersGame pos ext mov ⇒ GState pos ext → Random (Maybe mov)
computerMove' state
    | isLevelFinished state = pure Nothing
    | otherwise =
        let moves = possibleMoves state in
        let bestMove = (
            if state^._mode == RandomMode then
                Nothing
            else
                moves # find (maybe false isLosingPosition ∘ flip playAux state)
        ) in
            case bestMove of
                Just _ → pure bestMove
                Nothing → R.element' moves

data Objective = Minimize | Maximize
derive instance eqObjective ∷ Eq Objective 
data ShowWinPolicy = AlwaysShowWin | NeverShowWin | ShowWinOnNewRecord
derive instance eqSws ∷ Eq ShowWinPolicy

-- | classe pour les jeux consistant à minimiser ou maximiser un score
-- | permet d'indiquer le score de la partie en cours et de sauvegarder les meilleurs scores 
-- | 
-- | objective: détermine si c'est un jeu de minimisation ou maximisation
-- | scoreFn: renvoie un score (entier) en fonction de la position actuelle
-- | scoreHash: renvoie un string qui identifie une partie en fonction de ses paramètres
-- |           par exemple, pour le jeu reines, les paramètres sont la dimension de la grille et les pièces autorisées
-- | isCustomGame: détermine si la partie courante est une partie personnalisée
-- |               si c'est le cas, le score n'est pas conservé lorsque l'on créé une nouvelle partie
class Game pos ext mov <= ScoreGame pos ext mov | ext → pos mov  where
    objective ∷ GState pos ext → Objective
    scoreFn ∷ GState pos ext → Int
    scoreHash ∷  GState pos ext → String
    isCustomGame ∷ GState pos ext → Boolean                    

scoreHash' ∷ ∀pos ext mov. ScoreGame pos ext mov ⇒ GState pos ext → String
scoreHash' state 
    | isCustomGame state = "custom"
    | otherwise = scoreHash state

-- | implémentation de la fonction updateScore de la classe Game
-- | prend un argument supplémentaire de type ShowWinPolicy qui détermine si la popup de partie gagnée s'affiche ou non
-- | AlwaysShowWin: la popup s'affiche à chaque fois que l'on est dans une position gagnante (exemple: solitaire)
-- | NeverShowWin: n'affiche jamais de popup (exemple: jeu des reines)
-- | ShowWinOnNewRecord: affiche la popup seulement si le meilleur score a été battu (exemple: la bête)
updateScore' ∷ ∀pos ext mov. ScoreGame pos ext mov ⇒ ShowWinPolicy → GState pos ext → Tuple (GState pos ext) Boolean
updateScore' strat state =
    let score = scoreFn state
        hash = scoreHash' state 
        cmp = if objective state == Minimize then (<) else (>)
        oldScore = bestScore state
        isNewRecord = maybe true (cmp score ∘ fst) oldScore
        isNewRecord' = isNewRecord && strat == ShowWinOnNewRecord || strat == AlwaysShowWin
        st2 = state # _scores ∘ at hash %~ if isNewRecord then \_ → Just (score ∧ (state^._position)) else identity
    in st2 ∧ isNewRecord'

-- | renvoie le meilleur score pour la partie actuelle
-- | un meilleur score est une paire composée du score représenté par un entier et de la position témoignant du score
bestScore ∷ ∀pos ext mov. ScoreGame pos ext mov ⇒ GState pos ext → Maybe (Tuple Int pos)
bestScore state = state ^. _scores ∘ at (scoreHash' state)

data DndMsg i = Drag i | Drop i | Leave | DropOnBoard

class MsgWithDnd msg i | msg → i where
    dndmsg ∷ DndMsg i → msg

dndUpdate ∷ ∀pos ext i. Eq i ⇒ Game pos ext {from ∷ i, to ∷ i} ⇒ 
    Lens' (GState pos ext) (Maybe i) → DndMsg i → Update (GState pos ext)
dndUpdate _dragged (Drag i) = modify $ _dragged .~ Just i
dndUpdate _dragged (Drop i) = dropA _dragged i
dndUpdate _dragged Leave = modify $ _dragged .~ Nothing
dndUpdate _dragged DropOnBoard = modify $ _dragged .~ Nothing

dropA ∷ ∀pos ext dnd. Eq dnd ⇒  Game pos ext {from ∷ dnd, to ∷ dnd} ⇒
            Lens' (GState pos ext) (Maybe dnd) → dnd → Update (GState pos ext)
dropA dragLens to = do
    state ← get
    case state ^. dragLens of
        Nothing → pure unit
        Just drag → do
            modify (dragLens .~ Nothing)
            if drag /= to then playA { from: drag, to } else pure unit
