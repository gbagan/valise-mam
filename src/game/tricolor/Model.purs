module Game.Tricolor.Model where

import MyPrelude

import Game.Core (class Game, class MsgWithCore, GState, CoreMsg, coreUpdate, playA, _ext, genState, newGame, _position, defaultSizeLimit)
import Lib.Random as R
import Lib.Update (Update, modify)
import Lib.Util (abs)

-- une position est un tableau qui indique pour chaque sommmet la couleur du sommet
-- les couleurs sont comprises entre 0 et nbColors - 1
-- le but est de mettre toutes les couleurs à 0
-- une position initiale aléatoire n'est pas nécessairement une position gagnante
-- un coup (move) est représenté par le numéro du sommet que l'on souhaite activer

type Ext' = { 
    size ∷ Int,   -- le nombre de sommets
    nbColors ∷ Int,
    range ∷ Int,  -- le rayon autour du sommet activé pour lequel tous les sommets changent de couleurs
    hoverCell ∷ Maybe Int,
    shuffle ∷ Boolean
}
newtype ExtState = Ext Ext'
type State = GState (Array Int) ExtState

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_size ∷ Lens' State Int
_size = _ext' ∘ prop (SProxy ∷ _ "size")
_nbColors ∷ Lens' State Int
_nbColors = _ext' ∘ prop (SProxy ∷ _ "nbColors")
_range ∷ Lens' State Int
_range = _ext' ∘ prop (SProxy ∷ _ "range")
_shuffle ∷ Lens' State Boolean
_shuffle = _ext' ∘ prop (SProxy ∷ _ "shuffle")
_hoverCell ∷ Lens' State (Maybe Int)
_hoverCell = _ext' ∘ prop (SProxy ∷ _ "hoverCell")

-- | état initial
istate ∷ State
istate = genState [] identity (Ext { size: 5, nbColors: 2, range: 1, shuffle: false, hoverCell: Nothing })

-- | teste si le sommet i' va changer de couleur si on active le sommet i
inRange ∷ State → Int → Int → Boolean
inRange state i i' = min diff (state^._size - diff) <= state^._range
    where diff = abs (i - i')
    
instance tricolorGame ∷ Game (Array Int) ExtState Int where
    name _ = "tricolor"

    play state i = Just $ state^._position # mapWithIndex \i' color →
        if inRange state i i' then
            (color + 1) `mod` (state^._nbColors)
        else
            color
    initialPosition = pure <<< view _position
    isLevelFinished state = state^._position # all (_ == 0)

    onNewGame state = do
        position <- if state^._shuffle then 
                                replicateA (state^._size) (R.int' (state^._nbColors))
                            else
                                pure $ replicate (state^._size) 1
        pure $ state # set _position position # set _shuffle false

    computerMove _ = pure Nothing
    sizeLimit = defaultSizeLimit
    updateScore st = st ∧ true 
    onPositionChange = identity
    saveToJson _ = Nothing
    loadFromJson st _ = st


data Msg = Core CoreMsg | Play Int | SetSize Int | SetNbColors Int | SetRange Int | SetHoverCell (Maybe Int) | Shuffle
instance withcore ∷ MsgWithCore Msg where core = Core
  
update ∷ Msg → Update State
update (Core msg) = coreUpdate msg    
update (Play i) = playA i
update (SetSize size) = newGame $ set _size size
update (SetNbColors n) = newGame $ set _nbColors n
update (SetRange n) = newGame $ set _range n
update (SetHoverCell i) = modify $ set _hoverCell i
update Shuffle = newGame $ set _shuffle true