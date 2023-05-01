module Game.Tricolor.Model where

import MamPrelude
import Game.Core (class Game, class MsgWithCore, GModel, CoreMsg,
                   coreUpdate, playA, genModel, newGame, _ext, _position, defaultSizeLimit, defaultUpdateScore)
import Lib.Util (chooseInt')
import Lib.Update (UpdateMam)

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

newtype ExtModel = Ext Ext'
type Model = GModel (Array Int) ExtModel

-- lenses
_ext' ∷ Lens' Model Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_size ∷ Lens' Model Int
_size = _ext' ∘ prop (Proxy ∷ _ "size")
_nbColors ∷ Lens' Model Int
_nbColors = _ext' ∘ prop (Proxy ∷ _ "nbColors")
_range ∷ Lens' Model Int
_range = _ext' ∘ prop (Proxy ∷ _ "range")
_shuffle ∷ Lens' Model Boolean
_shuffle = _ext' ∘ prop (Proxy ∷ _ "shuffle")
_hoverCell ∷ Lens' Model (Maybe Int)
_hoverCell = _ext' ∘ prop (Proxy ∷ _ "hoverCell")

-- | état initial
imodel ∷ Model
imodel = genModel [] identity (Ext { size: 5, nbColors: 2, range: 1, shuffle: false, hoverCell: Nothing })

-- | teste si le sommet i' va changer de couleur si on active le sommet i
inRange ∷ Model → Int → Int → Boolean
inRange model i i' = min diff (model^._size - diff) <= model^._range
    where diff = abs (i - i')
    
instance Game Position ExtModel Move where
    name _ = "tricolor"

    play model i = Just $ model^._position # mapWithIndex \i' color →
        if inRange model i i' then
            (color + 1) `mod` (model^._nbColors)
        else
            color
    
    initialPosition model = if model^._shuffle then 
                                replicateA (model^._size) $ chooseInt' (model^._nbColors)
                            else
                                pure $ replicate (model^._size) 1
    
    isLevelFinished = all (_ == 0) ∘ view _position

    -- fonctions par défaut
    onNewGame = pure
    computerMove _ = pure Nothing
    sizeLimit = defaultSizeLimit
    updateScore s = defaultUpdateScore s
    onPositionChange = identity
    saveToJson _ = Nothing
    loadFromJson model _ = model


data Msg = Core CoreMsg | Play Int | SetSize Int | SetNbColors Int | SetRange Int | SetHoverCell (Maybe Int) | Shuffle
instance MsgWithCore Msg where core = Core
  
update ∷ Msg → UpdateMam Model Unit
update (Core msg) = coreUpdate msg
update (Play i) = playA i
update (SetSize size) = newGame $ set _size size
update (SetNbColors n) = newGame $ set _nbColors n
update (SetRange n) = newGame $ set _range n
update (SetHoverCell i) = _hoverCell .= i
update Shuffle = newGame $ over _shuffle not