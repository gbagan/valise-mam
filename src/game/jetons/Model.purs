module Game.Jetons.Model where

import MyPrelude
import Data.FoldableWithIndex (allWithIndex)
import Game.Core (class Game, class ScoreGame, class MsgWithCore, class MsgWithDnd,
                  CoreMsg, DndMsg, GState, SizeLimit(..), Objective(..), ShowWinPolicy(..),
                  coreUpdate, dndUpdate, _ext, genState, updateScore', _position, _nbColumns, _nbRows,
                  defaultOnNewGame, saveToJson', loadFromJson')
import Lib.Update (UpdateMam)
import Lib.Util (dCoords)

-- une position représente pour chaque numéro de case le nombre de jetons sur cette case
-- un coup (move) est du type {from, to} lorsque l'on souhaite déplacer une pile de jetons 
-- de la case de numéro from vers la case de numéro to

type Position = Array Int
type Ext' = { dragged ∷ Maybe Int }
newtype Ext = Ext Ext'
type State = GState Position Ext

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_dragged ∷ Lens' State (Maybe Int)
_dragged = _ext' ∘ prop (Proxy ∷ _ "dragged")

-- | état initial
istate ∷ State
istate = genState [] _{nbRows = 4, nbColumns = 4} (Ext { dragged: Nothing })

instance Game Position Ext { from ∷ Int, to ∷ Int } where
    name _ = "jetons"

    play state {from, to} = do
        let position = state^._position
        let {row, col} = dCoords (state^._nbColumns) from to
        pfrom ← position !! from
        pto ← position !! to
        guard $ pfrom > 0 && pfrom <= pto && row * row + col * col == 1
        position # updateAt from 0 >>= modifyAt to (_ + pfrom)
    
    initialPosition state = pure $ replicate (state^._nbRows * state^._nbColumns) 1

    isLevelFinished state =
        let position = state^._position
            columns = state^._nbColumns
        in
        position # allWithIndex \i x →
            let y = if (i+1) `mod` columns == 0 then 0 else position !! (i+1) ?: 0
                z = position !! (i+columns) ?: 0
            in x * (y + z) == 0

    sizeLimit _ = SizeLimit 1 2 6 12
    updateScore = updateScore' AlwaysShowWin
    saveToJson = saveToJson'
    loadFromJson = loadFromJson'

    computerMove _ = pure Nothing
    onNewGame = defaultOnNewGame
    onPositionChange = identity

instance ScoreGame Position Ext { from ∷ Int, to ∷ Int } where
    objective _ = Minimize
    scoreFn = length ∘ filter (_ > 0) ∘ view _position
    scoreHash state = show (state^._nbRows) <> "-" <> show (state^._nbColumns)
    isCustomGame _ = false

data Msg = Core CoreMsg | DnD (DndMsg Int)
instance MsgWithCore Msg where core = Core
instance MsgWithDnd Msg Int where dndmsg = DnD  

update ∷ Msg → UpdateMam State
update (Core msg) = coreUpdate msg
update (DnD msg) = dndUpdate _dragged msg
