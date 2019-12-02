module Game.Baseball.Model where

import MyPrelude
import Data.FoldableWithIndex (allWithIndex)
import Lib.Util ((..))
import Pha.Random (shuffle, randomInt)
import Pha.Action (Action)
import Game.Effs (EFFS)
import Game.Core (class Game, GState, class MsgWithCore, CoreMsg,
                 playA, coreUpdate, _ext, genState, newGame, _position, defaultSizeLimit)

type Ext' = { nbBases ∷ Int, missingPeg ∷ Int }
newtype ExtState = Ext Ext'
type State = GState (Array Int) ExtState

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_nbBases ∷ Lens' State Int
_nbBases = _ext' ∘ lens _.nbBases _{nbBases = _}
_missingPeg ∷ Lens' State Int
_missingPeg = _ext' ∘ lens _.missingPeg _{missingPeg = _}

-- | état initial
istate ∷ State
istate = genState [] identity (Ext { nbBases: 5, missingPeg: 0 })

instance baseballGame ∷ Game (Array Int) ExtState Int where
    play state i = do
        let position = state^._position
        let nbBases = state^._nbBases
        let j = state^._missingPeg
        x ← position !! i
        y ← position !! j
        if elem (x / 2 - y / 2) [1, nbBases-1, -1, 1-nbBases] then
            Just $ position # updateAtIndices [i ∧ y, j ∧ x]
        else 
            Nothing

    initialPosition state = shuffle $ 0 .. (2 * state^._nbBases - 1)
    isLevelFinished state = state^._position # allWithIndex \i j → i / 2 == j / 2
    onNewGame state = randomInt (2 * state^._nbBases) <#> \i → state # _missingPeg .~ i
    
    computerMove _ = pure Nothing
    sizeLimit = defaultSizeLimit
    updateScore st = st ∧ true 

data Msg = Core CoreMsg | SetNbBases Int | Play Int
instance withcore ∷ MsgWithCore Msg where core = Core

update ∷ Msg → Action State EFFS
update (Core msg) = coreUpdate msg
update (SetNbBases n) = newGame (_nbBases .~ n)
update (Play m) = playA m
