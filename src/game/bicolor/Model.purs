module Game.Bicolor.Model where

import MamPrelude
import Control.Monad.Gen (chooseBool)
import Game.Core
  ( class MsgWithCore
  , class Game
  , GModel
  , SizeLimit(..)
  , CoreMsg
  , _ext
  , coreUpdate
  , playA
  , _position
  , _nbColumns
  , _nbRows
  , newGame
  , genModel
  , defaultUpdateScore
  )
import Lib.Update (UpdateMam, evalGen)
import Lib.Helpers (dCoords)

data Card = BlackCard | WhiteCard | EmptyCard

derive instance eqCard ∷ Eq Card

data Mode = StandardMode | CylinderMode | TorusMode

derive instance eqMode ∷ Eq Mode

data Phase = PrepPhase | GamePhase

derive instance eqPhase ∷ Eq Phase

type Position = Array Card

reverseCard ∷ Card → Card
reverseCard BlackCard = WhiteCard
reverseCard WhiteCard = BlackCard
reverseCard EmptyCard = EmptyCard

randomCard ∷ Gen Card
randomCard = chooseBool # map if _ then WhiteCard else BlackCard

type Ext' =
  { mode ∷ Mode
  , phase ∷ Phase
  }

newtype ExtModel = Ext Ext'
type Model = GModel Position ExtModel

-- état initial
imodel ∷ Model
imodel = genModel [] _ { nbRows = 1, nbColumns = 8, customSize = true }
  ( Ext
      { mode: StandardMode, phase: GamePhase }
  )

-- lenses
_ext' ∷ Lens' Model Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext

_mode ∷ Lens' Model Mode
_mode = _ext' ∘ prop (Proxy ∷ _ "mode")

_phase ∷ Lens' Model Phase
_phase = _ext' ∘ prop (Proxy ∷ _ "phase")

neighbor ∷ Model → Int → Int → Boolean
neighbor model index1 index2 =
  row' * row' + col' * col' == 1
  where
  nbRows = model ^. _nbRows
  nbCols = model ^. _nbColumns
  { row, col } = dCoords nbCols index1 index2
  row' = if model ^. _mode == TorusMode && row ≠ 0 && abs row == nbRows - 1 then 1 else row
  col' = if model ^. _mode ≠ StandardMode && col ≠ 0 && abs col == nbCols - 1 then 1 else col

instance Game Position ExtModel Int where
  name _ = "bicolor"

  play model index =
    if model ^. _position !! index == Just WhiteCard then
      Just $ model ^. _position # mapWithIndex \i card →
        if i == index then
          EmptyCard
        else if neighbor model index i then
          reverseCard card
        else
          card
    else
      Nothing

  initialPosition model = pure $ replicate (model ^. _nbRows * model ^. _nbColumns) WhiteCard
  isLevelFinished model = all (_ ≠ WhiteCard) (model ^. _position)
  sizeLimit _ = SizeLimit 1 1 12 12

  -- méthodes par default
  onNewGame = pure
  saveToJson _ = Nothing
  loadFromJson model _ = model
  computerMove _ = pure Nothing
  updateScore s = defaultUpdateScore s
  onPositionChange = identity

data Msg = Core CoreMsg | Play Int | ToggleCard Int | SetMode Mode | ToggleCustom | Shuffle

instance MsgWithCore Msg where
  core = Core

update ∷ Msg → UpdateMam Model Msg Unit
update (Core msg) = coreUpdate msg
update (Play move) = playA move
update (ToggleCard i) = _position ∘ ix i %= reverseCard
update (SetMode mode) = newGame $ _mode .~ mode
update Shuffle = do
  model ← get
  pos ← evalGen $ replicateA (length $ model ^. _position) randomCard
  put $ model # _position .~ pos
update ToggleCustom = do
  model ← get
  if model ^. _phase == PrepPhase then
    _phase .= GamePhase
  else
    newGame $ _phase .~ PrepPhase
