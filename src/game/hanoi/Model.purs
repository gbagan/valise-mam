module Game.Hanoi.Model where

import MamPrelude
import Data.Array as Array
import Game.Core
  ( class Game
  , class MsgWithCore
  , class MsgWithDnd
  , CoreMsg
  , DndMsg
  , GModel
  , coreUpdate
  , dndUpdate
  , _ext
  , genModel
  , newGame
  , _position
  , defaultOnNewGame
  , defaultSizeLimit
  , defaultUpdateScore
  )
import Lib.Update (UpdateMam)

type Position = Array (Array Int)
type Ext' = { dragged ∷ Maybe Int, nbDisks ∷ Int }
newtype Ext = Ext Ext'
type Model = GModel Position Ext

-- lenses
_ext' ∷ Lens' Model Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext

_dragged ∷ Lens' Model (Maybe Int)
_dragged = _ext' ∘ prop (Proxy ∷ _ "dragged")

_nbDisks ∷ Lens' Model Int
_nbDisks = _ext' ∘ prop (Proxy ∷ _ "nbDisks")

-- | état initial
imodel ∷ Model
imodel = genModel [] identity (Ext { dragged: Nothing, nbDisks: 4 })

instance Game Position Ext { from ∷ Int, to ∷ Int } where
  name _ = "hanoi"

  play model { from, to } = do
    let position = model ^. _position
    { init, last } <- position !! from >>= Array.unsnoc
    guard $ from /= to && Just last > (position !! to >>= Array.last)
    pure $ position # ix from .~ init
      # ix to
      %~ (_ `snoc` last)

  initialPosition model = pure [ 0 .. (model ^. _nbDisks - 1), [], [] ]

  isLevelFinished model = case model ^. _position of
    [ [], [], _ ] → true
    _ → false

  -- fonctions par défaut
  onNewGame = defaultOnNewGame
  sizeLimit = defaultSizeLimit
  computerMove _ = pure Nothing
  onPositionChange = identity
  updateScore s = defaultUpdateScore s
  saveToJson _ = Nothing
  loadFromJson model _ = model

data Msg = Core CoreMsg | DnD (DndMsg Int) | SetNbDisks Int

instance MsgWithCore Msg where
  core = Core

instance MsgWithDnd Msg Int where
  dndmsg = DnD

update ∷ Msg → UpdateMam Model Msg Unit
update (Core msg) = coreUpdate msg
update (DnD msg) = dndUpdate _dragged msg
update (SetNbDisks n) = newGame $ set _nbDisks n