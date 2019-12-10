module Game.Tricolor.Model where

import MyPrelude
import Lib.Util (abs)
import Pha.Random (randomInt)
import Pha.Update (Update, purely)
import Game.Effs (EFFS)
import Game.Core (class Game, class MsgWithCore, GState, CoreMsg,
                coreUpdate, playA, _ext, genState, newGame, _position, defaultSizeLimit)

-- une position est un tableau qui indique pour chaque sommmet la couleur du sommet
-- les couleurs sont comprises entre 0 et nbColors - 1
-- le but est de mettre toutes les couleurs à 0
-- une position initiale aléatoire n'est pas nécessairement une position gagnante
-- un coup (move) est représenté par le numéro du sommet que l'on souhaite activer

type Ext' = { 
    size ∷ Int,   -- le nombre de sommets
    nbColors ∷ Int,
    range ∷ Int,  -- le rayon autour du sommet activé pour lequel tous les sommets changent de couleurs
    hoverCell ∷ Maybe Int
}
newtype ExtState = Ext Ext'
type State = GState (Array Int) ExtState

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_size ∷ Lens' State Int
_size = _ext' ∘ lens _.size _{size = _}
_nbColors ∷ Lens' State Int
_nbColors = _ext' ∘ lens _.nbColors _{nbColors = _}
_range ∷ Lens' State Int
_range = _ext' ∘ lens _.range _{range = _}
_hoverCell ∷ Lens' State (Maybe Int)
_hoverCell = _ext' ∘ lens _.hoverCell _{hoverCell = _}

-- état initial
istate ∷ State
istate = genState [] identity (Ext { size: 5, nbColors: 2, range: 1, hoverCell: Nothing })

-- teste si le sommet i' va changer de couleur si on active le sommet i
inRange ∷ State → Int → Int → Boolean
inRange state i i' = min diff (state^._size - diff) <= state^._range
    where diff = abs (i - i')
    
instance tricolorGame ∷ Game (Array Int) ExtState Int where
    play state i = Just $ state^._position # mapWithIndex \i' color →
        if inRange state i i' then
            (color + 1) `mod` (state^._nbColors)
        else
            color
    initialPosition state = sequence $ replicate (state^._size) $ randomInt (state^._nbColors)
    isLevelFinished state = state^._position # all (_ == 0)

    onNewGame = pure
    computerMove _ = pure Nothing
    sizeLimit = defaultSizeLimit
    updateScore st = st ∧ true 

data Msg = Core CoreMsg | Play Int | SetSize Int | SetNbColors Int | SetRange Int | SetHoverCell (Maybe Int)
instance withcore ∷ MsgWithCore Msg where core = Core
  
update ∷ Msg → Update State EFFS
update (Core msg) = coreUpdate msg    
update (Play i) = playA i
update (SetSize size) = newGame $ _size .~ size
update (SetNbColors n) = newGame $ _nbColors .~ n
update (SetRange n) = newGame $ _range .~ n
update (SetHoverCell i) = purely $ _hoverCell .~ i
