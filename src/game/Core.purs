module Game.Core where

import MamPrelude

import Control.Monad.Gen.Trans (suchThat)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.List as List
import Data.Map as Map
import Game.Common (releasePointerCapture, pointerDecoder)
import Lib.Update (UpdateMam, evalGen, delay, storageGet, storagePut)
import Lib.Util (elements')
import Web.Event.Event (stopPropagation)
import Web.PointerEvent (PointerEvent)
import Web.PointerEvent.PointerEvent as PE

-- ConfirmNewGame contient le futur état d'une nouvelle partie
data Dialog a = Rules | NoDialog | ConfirmNewGameDialog a | ScoreDialog | CustomDialog

data Mode = SoloMode | RandomMode | ExpertMode | DuelMode

derive instance Eq Mode

data Turn = Turn1 | Turn2

derive instance Eq Turn

type PointerPosition = { x ∷ Number, y ∷ Number }

type CoreModel pos ext =
  { position ∷ pos
  , history ∷ List pos
  , redoHistory ∷ List pos
  , dialog ∷ Dialog (GModel pos ext)
  , turn ∷ Turn
  , nbRows ∷ Int
  , nbColumns ∷ Int
  , customSize ∷ Boolean
  , mode ∷ Mode --- mode pour les jeux à deux joueurs
  , help ∷ Boolean --- si l'aide est activée ou non
  , locked ∷ Boolean --- quand locked est à true, aucune action de l'utiliateur n'est possible
  , showWin ∷ Boolean
  , scores ∷ Map String (Tuple Int pos)
  , pointer ∷ Maybe PointerPosition --- position du pointeur en % relativement au plateau de jeu
  }

data GModel pos ext = Model (CoreModel pos ext) ext

defaultCoreModel ∷ ∀ pos ext. pos → CoreModel pos ext
defaultCoreModel p =
  { position: p
  , history: Nil
  , redoHistory: Nil
  , dialog: Rules
  , turn: Turn1
  , nbRows: 0
  , nbColumns: 0
  , customSize: false
  , help: false
  , mode: SoloMode
  , locked: false
  , showWin: false
  , scores: Map.empty
  , pointer: Nothing
  }

-- fonction pour faciliter la création d'un état initial
genModel ∷ ∀ pos ext. pos → (CoreModel pos ext → CoreModel pos ext) → ext → GModel pos ext
genModel p f ext = Model (f $ defaultCoreModel p) ext

-- lenses 
_core ∷ ∀ pos ext. Lens' (GModel pos ext) (CoreModel pos ext)
_core = lens (\(Model c _) → c) \(Model _ e) c → Model c e

_ext ∷ ∀ pos ext. Lens' (GModel pos ext) ext
_ext = lens (\(Model _ e) → e) \(Model c _) e → Model c e

_position ∷ ∀ pos ext. Lens' (GModel pos ext) pos
_position = _core ∘ prop (Proxy ∷ _ "position")

_history ∷ ∀ pos ext. Lens' (GModel pos ext) (List pos)
_history = _core ∘ prop (Proxy ∷ _ "history")

_redoHistory ∷ ∀ pos ext. Lens' (GModel pos ext) (List pos)
_redoHistory = _core ∘ prop (Proxy ∷ _ "redoHistory")

_mode ∷ ∀ pos ext. Lens' (GModel pos ext) Mode
_mode = _core ∘ prop (Proxy ∷ _ "mode")

_help ∷ ∀ pos ext. Lens' (GModel pos ext) Boolean
_help = _core ∘ prop (Proxy ∷ _ "help")

_turn ∷ ∀ pos ext. Lens' (GModel pos ext) Turn
_turn = _core ∘ prop (Proxy ∷ _ "turn")

_dialog ∷ ∀ pos ext. Lens' (GModel pos ext) (Dialog (GModel pos ext))
_dialog = _core ∘ prop (Proxy ∷ _ "dialog")

_nbRows ∷ ∀ pos ext. Lens' (GModel pos ext) Int
_nbRows = _core ∘ prop (Proxy ∷ _ "nbRows")

_nbColumns ∷ ∀ pos ext. Lens' (GModel pos ext) Int
_nbColumns = _core ∘ prop (Proxy ∷ _ "nbColumns")

_customSize ∷ ∀ pos ext. Lens' (GModel pos ext) Boolean
_customSize = _core ∘ prop (Proxy ∷ _ "customSize")

_locked ∷ ∀ pos ext. Lens' (GModel pos ext) Boolean
_locked = _core ∘ prop (Proxy ∷ _ "locked")

_showWin ∷ ∀ pos ext. Lens' (GModel pos ext) Boolean
_showWin = _core ∘ prop (Proxy ∷ _ "showWin")

_pointer ∷ ∀ pos ext. Lens' (GModel pos ext) (Maybe PointerPosition)
_pointer = _core ∘ prop (Proxy ∷ _ "pointer")

_scores ∷ ∀ pos ext. Lens' (GModel pos ext) (Map String (Tuple Int pos))
_scores = _core ∘ prop (Proxy ∷ _ "scores")

data SizeLimit = SizeLimit Int Int Int Int

class Game pos ext mov | ext → pos mov where
  name ∷ GModel pos ext → String
  play ∷ GModel pos ext → mov → Maybe pos
  initialPosition ∷ GModel pos ext → Gen pos
  isLevelFinished ∷ GModel pos ext → Boolean
  sizeLimit ∷ GModel pos ext → SizeLimit
  computerMove ∷ GModel pos ext → Gen (Maybe mov)
  onNewGame ∷ GModel pos ext → Gen (GModel pos ext)
  onPositionChange ∷ GModel pos ext → GModel pos ext
  updateScore ∷ GModel pos ext → { newModel :: GModel pos ext, isNewRecord :: Boolean, showWin :: Boolean }
  saveToJson ∷ GModel pos ext → Maybe Json
  loadFromJson ∷ GModel pos ext → Json → GModel pos ext

-- | implémentation de saveToJson pour les jeux à score
saveToJson'
  ∷ ∀ pos ext
  . EncodeJson pos
  ⇒ GModel pos ext
  → Maybe Json
saveToJson' = Just ∘ encodeJson ∘ Map.delete "custom" ∘ view _scores

-- | implémentation de loadFromJson' pour les jeux à score
loadFromJson'
  ∷ ∀ pos ext
  . DecodeJson pos
  ⇒ GModel pos ext
  → Json
  → GModel pos ext
loadFromJson' model json =
  case decodeJson json of
    Left _ → model
    Right scores → model # set _scores scores

canPlay ∷ ∀ pos ext mov. Game pos ext mov ⇒ GModel pos ext → mov → Boolean
canPlay model mov = isJust (play model mov)

defaultSizeLimit ∷ ∀ a. a → SizeLimit
defaultSizeLimit _ = SizeLimit 0 0 0 0

defaultOnNewGame ∷ ∀ a. a → Gen a
defaultOnNewGame = pure

defaultUpdateScore ∷ ∀ pos ext move. Game pos ext move ⇒ GModel pos ext → { newModel :: GModel pos ext, isNewRecord :: Boolean, showWin :: Boolean }
defaultUpdateScore s = { newModel: s, isNewRecord: false, showWin: isLevelFinished s }

oppositeTurn ∷ Turn → Turn
oppositeTurn Turn1 = Turn2
oppositeTurn _ = Turn1

changeTurn ∷ ∀ pos ext. GModel pos ext → GModel pos ext
changeTurn model = model # over _turn \x → if model ^. _mode == DuelMode then oppositeTurn x else Turn1

data CoreMsg
  = Undo
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
  | SetPointer PointerEvent
  | SetPointerToNothing
  | ComputerStarts
  | Init

class MsgWithCore a where
  core ∷ CoreMsg → a

coreUpdate ∷ ∀ pos ext mov msg. Game pos ext mov ⇒ CoreMsg → UpdateMam (GModel pos ext) msg Unit
coreUpdate Undo = modify_ \model → case model ^. _history of
  Nil → model
  Cons h rest →
    model # changeTurn
      # set _position h
      # set _history rest
      # over _redoHistory (Cons (model ^. _position))
      # onPositionChange

coreUpdate Redo = modify_ \model → case model ^. _redoHistory of
  Nil → model
  Cons h rest →
    model # changeTurn
      # set _position h
      # set _redoHistory rest
      # over _history (Cons (model ^. _position))
      # onPositionChange

coreUpdate Reset = modify_ \model → case List.last (model ^. _history) of
  Nothing → model
  Just x → model # set _position x
    # set _history Nil
    # set _redoHistory Nil
    # set _turn Turn1
    # onPositionChange
coreUpdate Clear = newGame identity
coreUpdate ToggleHelp = modify_ $ over _help not
coreUpdate (SetMode mode) = newGame (set _mode mode)
coreUpdate (SetGridSize nbRows nbColumns customSize) =
  newGame $ setSize' ∘ (set _customSize customSize)
  where
  setSize' model =
    if nbRows >= minrows && nbRows <= maxrows && nbColumns >= mincols && nbColumns <= maxcols then
      model # set _nbRows nbRows # set _nbColumns nbColumns
    else
      model
    where
    SizeLimit minrows mincols maxrows maxcols = sizeLimit model
coreUpdate (SetCustomSize bool) = _customSize .= bool
coreUpdate SetNoDialog = _dialog .= NoDialog
coreUpdate SetRulesDialog = _dialog .= Rules
coreUpdate SetScoreDialog = _dialog .= ScoreDialog
coreUpdate ConfirmNewGame = modify_ \model →
  case model ^. _dialog of
    ConfirmNewGameDialog model' → model'
    _ → model
coreUpdate (SetPointer ev) = do
  pos ← liftEffect $ pointerDecoder (PE.toMouseEvent ev)
  when (isJust pos) (_pointer .= pos)
coreUpdate SetPointerToNothing = _pointer .= Nothing
coreUpdate ComputerStarts = do
  modify_ $ pushToHistory >>> over _turn oppositeTurn
  computerPlay
coreUpdate Init = do
  newGame identity
  model ← get
  val ← storageGet ("valise-" <> name model)
  case val of
    Nothing → pure unit
    Just text →
      case jsonParser text of
        Left _ → pure unit
        Right json → do
          put $ loadFromJson model json

playAux ∷ ∀ pos ext mov. Game pos ext mov ⇒ mov → GModel pos ext → Maybe (GModel pos ext)
playAux move model = do
  pos ← play model move
  pure $ model # set _position pos
    # over _turn oppositeTurn
    # onPositionChange

-- met dans l'historique la position actuelle
pushToHistory ∷ ∀ pos ext. GModel pos ext → GModel pos ext
pushToHistory model = model
  # over _history (Cons $ model ^. _position)
  # set _redoHistory Nil

showVictory ∷ ∀ pos ext msg. UpdateMam (GModel pos ext) msg Unit
showVictory = do
  _showWin .= true
  delay (Milliseconds 1000.0)
  _showWin .= false

computerPlay ∷ ∀ pos ext mov msg. Game pos ext mov ⇒ UpdateMam (GModel pos ext) msg Unit
computerPlay = do
  model ← get
  move ← evalGen $ computerMove model
  for_ (flip playAux model =<< move) \model2 → do
    put model2
    when (isLevelFinished model2) showVictory

saveToStorage ∷ ∀ pos ext mov msg. Game pos ext mov ⇒ UpdateMam (GModel pos ext) msg Unit
saveToStorage = do
  model ← get
  for_ (saveToJson model) \json →
    storagePut ("valise-" <> name model) (stringify json)

playA ∷ ∀ pos ext mov msg. Game pos ext mov ⇒ mov → UpdateMam (GModel pos ext) msg Unit
playA move = lockAction $ do
  model ← get
  for_ (playAux move $ pushToHistory $ model) \st2 → do
    let { newModel: st3, isNewRecord, showWin } = updateScore st2
    put st3
    when isNewRecord saveToStorage
    if showWin then
      showVictory
    else when ((st3 ^. _mode) `elem` [ ExpertMode, RandomMode ]) do
      delay (Milliseconds 1000.0)
      computerPlay

-- | Empêche d'autres actions d'être effectués durant la durée de l'action
-- | en mettant locked à true au début de l'action et à false à la fin de l'action.
-- | L'action n'est pas executé si locked est déjà à true
lockAction ∷ ∀ pos ext msg. UpdateMam (GModel pos ext) msg Unit → UpdateMam (GModel pos ext) msg Unit
lockAction action = unlessM (view _locked <$> get) do
  _locked .= true
  action
  _locked .= false

-- | fonction auxiliaire pour newGame
newGameAux
  ∷ ∀ pos ext mov
  . Game pos ext mov
  ⇒ (GModel pos ext → GModel pos ext)
  → GModel pos ext
  → Gen (GModel pos ext)
newGameAux f model = do
  let model2 = f model
  model3 ← onNewGame model2
  position ← initialPosition model3 `suchThat` \p -> not $ isLevelFinished $ model3 # set _position p
  pure $ model3
    # set _position position
    # set _history Nil
    # set _redoHistory Nil
    # set _help false
    # set _turn Turn1
    # set (_scores ∘ at "custom") Nothing

-- | créé une nouvelle partie en applicant la fonction f donnée en argument.
-- | Par défault, ouvre une boite de dialogue pour confirmer la nouvelle partie
-- | L'état qu'il y aura après une nouvelle partie est stockée dans ConfirmNewGameDialog s
-- | Si l'utilisateur confirme, on remplace l'état courant par s
-- | Si l'historique est vide ou si la partie est finie, on lance une nouvelle partie
-- | sans demander confirmation à l'utilisateur
newGame
  ∷ ∀ pos ext mov msg
  . Game pos ext mov
  ⇒ (GModel pos ext → GModel pos ext)
  → UpdateMam (GModel pos ext) msg Unit
newGame f = do
  model <- get
  rmodel <- evalGen $ newGameAux f model
  put $
    if List.null (model ^. _history) || isLevelFinished model then
      rmodel
    else
      rmodel # \s → model # set _dialog (ConfirmNewGameDialog s)

-- | classe facilitant l'implétentation d'une IA pour les jeux à deux joueurs
-- | nécessite de pouvoir calculer efficacement l'ensemble des coups légaux pour un joueur
-- | et déterminer si une position est perdante ou non
class Game pos ext mov <= TwoPlayersGame pos ext mov | ext → pos mov where
  isLosingPosition ∷ GModel pos ext → Boolean
  possibleMoves ∷ GModel pos ext → Array mov

-- | implémentation de la fonction computerMove de la classe Game
-- | nécessite l'implémentation de la classe TwoPlayersGame
computerMove' ∷ ∀ pos ext mov. TwoPlayersGame pos ext mov ⇒ GModel pos ext → Gen (Maybe mov)
computerMove' model
  | isLevelFinished model = pure Nothing
  | otherwise =
      let
        moves = possibleMoves model
        bestMove =
          if model ^. _mode == RandomMode then
            Nothing
          else
            moves # find (maybe false isLosingPosition ∘ flip playAux model)
      in
        case bestMove of
          Just _ → pure bestMove
          Nothing → elements' moves

data Objective = Minimize | Maximize

derive instance Eq Objective
data ShowWinPolicy = AlwaysShowWin | NeverShowWin | ShowWinOnNewRecord

derive instance Eq ShowWinPolicy

-- | classe pour les jeux consistant à minimiser ou maximiser un score
-- | permet d'indiquer le score de la partie en cours et de sauvegarder les meilleurs scores 
-- | 
-- | objective: détermine si c'est un jeu de minimisation ou maximisation
-- | scoreFn: renvoie un score (entier) en fonction de la position actuelle
-- | scoreHash: renvoie un string qui identifie une partie en fonction de ses paramètres
-- |           par exemple, pour le jeu reines, les paramètres sont la dimension de la grille et les pièces autorisées
-- | isCustomGame: détermine si la partie courante est une partie personnalisée
-- |               si c'est le cas, le score n'est pas conservé lorsque l'on créé une nouvelle partie
class Game pos ext mov <= ScoreGame pos ext mov | ext → pos mov where
  objective ∷ GModel pos ext → Objective
  scoreFn ∷ GModel pos ext → Int
  scoreHash ∷ GModel pos ext → String
  isCustomGame ∷ GModel pos ext → Boolean

scoreHash' ∷ ∀ pos ext mov. ScoreGame pos ext mov ⇒ GModel pos ext → String
scoreHash' model
  | isCustomGame model = "custom"
  | otherwise = scoreHash model

-- | implémentation de la fonction updateScore de la classe Game
-- | prend un argument supplémentaire de type ShowWinPolicy qui détermine si la popup de partie gagnée s'affiche ou non
-- | AlwaysShowWin: la popup s'affiche à chaque fois que l'on est dans une position gagnante (exemple: solitaire)
-- | NeverShowWin: n'affiche jamais de popup (exemple: jeu des reines)
-- | ShowWinOnNewRecord: affiche la popup seulement si le meilleur score a été battu (exemple: la bête)
updateScore'
  ∷ ∀ pos ext mov
  . ScoreGame pos ext mov
  ⇒ { onlyWhenFinished :: Boolean, showWin :: ShowWinPolicy }
  → GModel pos ext
  → { newModel :: (GModel pos ext), isNewRecord :: Boolean, showWin :: Boolean }
updateScore' { onlyWhenFinished, showWin } model =
  if onlyWhenFinished && not (isLevelFinished model) then
    { newModel: model, isNewRecord: false, showWin: false }
  else
    let
      score = scoreFn model
      hash = scoreHash' model
      cmp = if objective model == Minimize then (<) else (>)
      oldScore = bestScore model
      isNewRecord = maybe true (cmp score ∘ fst) oldScore
    in
      { newModel:
          if isNewRecord then
            model # (_scores ∘ at hash) .~ Just (score ∧ (model ^. _position))
          else
            model
      , isNewRecord
      , showWin: isNewRecord && showWin == ShowWinOnNewRecord || showWin == AlwaysShowWin
      }

-- | renvoie le meilleur score pour la partie actuelle
-- | un meilleur score est une paire composée du score représenté par un entier et de la position témoignant du score
bestScore ∷ ∀ pos ext mov. ScoreGame pos ext mov ⇒ GModel pos ext → Maybe (Tuple Int pos)
bestScore model = model ^. _scores ∘ at (scoreHash' model)

data DndMsg i
  = Drag Boolean i PointerEvent
  | Drop Boolean i PointerEvent
  | Leave
  | DropOnBoard

class MsgWithDnd msg i | msg → i where
  dndmsg ∷ DndMsg i → msg

dndUpdate
  ∷ ∀ pos ext i msg
  . Eq i
  ⇒ Game pos ext { from ∷ i, to ∷ i }
  ⇒ Lens' (GModel pos ext) (Maybe i)
  → DndMsg i
  → UpdateMam (GModel pos ext) msg Unit
dndUpdate _dragged (Drag draggable i ev) = do
  liftEffect $ releasePointerCapture ev
  when draggable (_dragged .= Just i)
dndUpdate _dragged (Drop candrop i ev) =
  when candrop do
    liftEffect $ stopPropagation $ PE.toEvent ev
    dropA _dragged i
dndUpdate _dragged Leave = _dragged .= Nothing
dndUpdate _dragged DropOnBoard = _dragged .= Nothing

dropA
  ∷ ∀ pos ext dnd msg
  . Eq dnd
  ⇒ Game pos ext { from ∷ dnd, to ∷ dnd }
  ⇒ Lens' (GModel pos ext) (Maybe dnd)
  → dnd
  → UpdateMam (GModel pos ext) msg Unit
dropA dragLens to = do
  model ← get
  for_ (model ^. dragLens) \drag → do
    modify_ (set dragLens Nothing)
    when (drag ≠ to) $ playA { from: drag, to }
