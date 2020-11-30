module Game.Baseball.Model where

import MyPrelude
import Data.FoldableWithIndex (allWithIndex)
import Lib.Util ((..))
import Lib.Random as R
import Lib.Update (Update)
import Game.Core (class Game, GState, class MsgWithCore, CoreMsg,
                 playA, coreUpdate, _ext, genState, newGame, _position, defaultSizeLimit)

-- les jetons sont numérotés de 0 à nbBases - 1
-- un jeton de numéro i a la couleur i / 2 (division entière)
-- la "position" est un tableau qui associe à un numéro de joueur sa position
-- les positions 0 et 1 sont sur la première base, les numéros 2 et 3 sur la seconde, etc
-- un coup (move) est représenté par le numéro du jeton que l'on souhaite déplacer
-- une position est gagnante si tous les jetons sont sur la base de la même couleur
-- c'est à dire que pour tout jeton de numéro i à la position j, on a i / 2 = j / 2 (division entière)

-- attributs supplémentaires
type Ext' =
    { nbBases ∷ Int     -- le nombre de bases
    , missingPeg ∷ Int  -- le numéro du jeton manquant
    }
newtype Ext = Ext Ext'
type State = GState (Array Int) Ext

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_nbBases ∷ Lens' State Int
_nbBases = _ext' ∘ prop (SProxy ∷ _ "nbBases")
_missingPeg ∷ Lens' State Int
_missingPeg = _ext' ∘ prop (SProxy ∷ _ "missingPeg")

-- | état initial
istate ∷ State
istate =
    genState [] identity (Ext
        {   nbBases: 5
        ,   missingPeg: 0
        }
    )

instance game ∷ Game (Array Int) Ext Int where
    name _ = "baseball"

    play state i = do
        let position = state ^. _position
        let nbBases = state ^. _nbBases
        let j = state ^. _missingPeg
        x ← position !! i
        y ← position !! j
        guard $ elem (x / 2 - y / 2) [1, nbBases-1, -1, 1-nbBases]
        Just $ position # updateAtIndices [i ∧ y, j ∧ x]

    initialPosition state = R.shuffle (0 .. (2 * state ^. _nbBases - 1))
    isLevelFinished state = state ^. _position # allWithIndex \i j → i / 2 == j / 2
    -- on tire aléatoirement le jeton manquant
    onNewGame state = R.int' (2 * state ^. _nbBases) <#> \i → state # set _missingPeg i
    
    -- fonctions par défault
    computerMove _ = pure Nothing
    sizeLimit = defaultSizeLimit
    updateScore st = st ∧ true 
    onPositionChange = identity
    saveToJson _ = Nothing
    loadFromJson st _ = st

data Msg = Core CoreMsg | SetNbBases Int | Play Int
instance withcore ∷ MsgWithCore Msg where core = Core

update ∷ Msg → Update State
update (Core msg) = coreUpdate msg
update (SetNbBases n) = newGame $ set _nbBases n
update (Play m) = playA m
