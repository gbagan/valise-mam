module Game.Tricolor.Model where

import MyPrelude
import Game.Core (class Game, class MsgWithCore, GState, CoreMsg,
                   coreUpdate, playA, genState, newGame, _ext, _position, defaultSizeLimit)
import Lib.Random as Random
import Lib.Update (Update)

-- une position est un tableau qui indique pour chaque sommmet la couleur du sommet
-- les couleurs sont comprises entre 0 et nbColors - 1
-- le but est de mettre toutes les couleurs à 0
-- une position initiale aléatoire n'est pas nécessairement une position gagnante
-- un coup (move) est représenté par le numéro du sommet que l'on souhaite activer

type Position = Array Int
type Move = Int

type Ext' =
    {   size ∷ Int      -- le nombre de sommets
    ,   nbColors ∷ Int
    ,   range ∷ Int     -- le rayon autour du sommet activé pour lequel tous les sommets changent de couleurs
    ,   hoverCell ∷ Maybe Int
    ,   shuffle ∷ Boolean
    }

newtype ExtState = Ext Ext'
type State = GState (Array Int) ExtState

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_size ∷ Lens' State Int
_size = _ext' ∘ prop (Proxy ∷ _ "size")
_nbColors ∷ Lens' State Int
_nbColors = _ext' ∘ prop (Proxy ∷ _ "nbColors")
_range ∷ Lens' State Int
_range = _ext' ∘ prop (Proxy ∷ _ "range")
_shuffle ∷ Lens' State Boolean
_shuffle = _ext' ∘ prop (Proxy ∷ _ "shuffle")
_hoverCell ∷ Lens' State (Maybe Int)
_hoverCell = _ext' ∘ prop (Proxy ∷ _ "hoverCell")

-- | état initial
istate ∷ State
istate = genState [] identity (Ext { size: 5, nbColors: 2, range: 1, shuffle: false, hoverCell: Nothing })

-- | teste si le sommet i' va changer de couleur si on active le sommet i
inRange ∷ State → Int → Int → Boolean
inRange state i i' = min diff (state^._size - diff) <= state^._range
    where diff = abs (i - i')
    
instance Game Position ExtState Move where
    name _ = "tricolor"

    play state i = Just $ state^._position # mapWithIndex \i' color →
        if inRange state i i' then
            (color + 1) `mod` (state^._nbColors)
        else
            color
    
    initialPosition state = if state^._shuffle then 
                                Random.arrayOf (state^._size) (Random.int' (state^._nbColors))
                            else
                                pure $ replicate (state^._size) 1
    
    isLevelFinished = all (_ == 0) ∘ view _position

    -- fonctions par défaut
    onNewGame = pure
    computerMove _ = pure Nothing
    sizeLimit = defaultSizeLimit
    updateScore st = st ∧ true
    onPositionChange = identity
    saveToJson _ = Nothing
    loadFromJson st _ = st


data Msg = Core CoreMsg | Play Int | SetSize Int | SetNbColors Int | SetRange Int | SetHoverCell (Maybe Int) | Shuffle
instance MsgWithCore Msg where core = Core
  
update ∷ Msg → Update State Unit
update (Core msg) = coreUpdate msg
update (Play i) = playA i
update (SetSize size) = newGame $ set _size size
update (SetNbColors n) = newGame $ set _nbColors n
update (SetRange n) = newGame $ set _range n
update (SetHoverCell i) = _hoverCell .= i
update Shuffle = newGame $ over _shuffle not