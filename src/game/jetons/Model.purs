module Game.Jetons.Model where

import MamPrelude

import Data.FoldableWithIndex (allWithIndex)
import Game.Core (class Game, class MsgWithCore, class MsgWithDnd, class ScoreGame, CoreMsg, DndMsg, GModel, Objective(..), ShowWinPolicy(..), SizeLimit(..), _ext, _nbColumns, _nbRows, _position, coreUpdate, defaultOnNewGame, dndUpdate, genModel, loadFromJson', saveToJson', updateScore')
import Lib.Update (UpdateMam)
import Lib.Util (count, dCoords)

-- une position représente pour chaque numéro de case le nombre de jetons sur cette case
-- un coup (move) est du type {from, to} lorsque l'on souhaite déplacer une pile de jetons 
-- de la case de numéro from vers la case de numéro to

type Position = Array Int
type Ext' = { dragged ∷ Maybe Int }
newtype Ext = Ext Ext'
type Model = GModel Position Ext

-- lenses
_ext' ∷ Lens' Model Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext

_dragged ∷ Lens' Model (Maybe Int)
_dragged = _ext' ∘ prop (Proxy ∷ _ "dragged")

-- | état initial
imodel ∷ Model
imodel = genModel [] _ { nbRows = 4, nbColumns = 4 } (Ext { dragged: Nothing })

instance Game Position Ext { from ∷ Int, to ∷ Int } where
  name _ = "jetons"

  play model { from, to } = do
    let position = model ^. _position
    let { row, col } = dCoords (model ^. _nbColumns) from to
    pfrom ← position !! from
    pto ← position !! to
    guard $ pfrom > 0 && pfrom <= pto && row * row + col * col == 1
    position # updateAt from 0 >>= modifyAt to (_ + pfrom)

  initialPosition model = pure $ replicate (model ^. _nbRows * model ^. _nbColumns) 1

  isLevelFinished model =
    let
      position = model ^. _position
      columns = model ^. _nbColumns
    in
      position # allWithIndex \i x →
        let
          y = if (i + 1) `mod` columns == 0 then 0 else position !! (i + 1) ?: 0
          z = position !! (i + columns) ?: 0
        in
          x * (y + z) == 0

  sizeLimit _ = SizeLimit 1 2 6 12
  updateScore model = updateScore' { onlyWhenFinished: true, showWin: AlwaysShowWin } model
  saveToJson = saveToJson'
  loadFromJson = loadFromJson'

  computerMove _ = pure Nothing
  onNewGame = defaultOnNewGame
  onPositionChange = identity

instance ScoreGame Position Ext { from ∷ Int, to ∷ Int } where
  objective _ = Minimize
  scoreFn = count (_ > 0) ∘ view _position
  scoreHash model = show (model ^. _nbRows) <> "-" <> show (model ^. _nbColumns)
  isCustomGame _ = false

data Msg = Core CoreMsg | DnD (DndMsg Int)

instance MsgWithCore Msg where
  core = Core

instance MsgWithDnd Msg Int where
  dndmsg = DnD

update ∷ Msg → UpdateMam Model Msg Unit
update (Core msg) = coreUpdate msg
update (DnD msg) = dndUpdate _dragged msg
