module Game.Baseball.Model where

import MamPrelude
import Data.FoldableWithIndex (allWithIndex)
import Control.Monad.Gen.Trans (shuffle)
import Lib.Util (chooseInt')
import Lib.Update (UpdateMam)
import Game.Core (class Game, GModel, class MsgWithCore, CoreMsg,
                 playA, coreUpdate, _ext, genModel, newGame, _position,
                 defaultSizeLimit, defaultUpdateScore
                 )

-- les jetons sont numérotés de 0 à nbBases - 1
-- un jeton de numéro i a la couleur i / 2 (division entière)
-- la "position" est un tableau qui associe à un numéro de joueur sa position
-- les positions 0 et 1 sont sur la première base, les numéros 2 et 3 sur la seconde, etc
-- un coup (move) est représenté par le numéro du jeton que l'on souhaite déplacer
-- une position est gagnante si tous les jetons sont sur la base de la même couleur
-- c'est à dire que pour tout jeton de numéro i à la position j, on a i / 2 = j / 2 (division entière)
type Position = Array Int
type Move = Int

-- attributs supplémentaires
type Ext' =
  { nbBases ∷ Int     -- le nombre de bases
  , missingPeg ∷ Int  -- le numéro du jeton manquant
  }

newtype Ext = Ext Ext'
type Model = GModel (Array Int) Ext

-- lenses
_ext' ∷ Lens' Model Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_nbBases ∷ Lens' Model Int
_nbBases = _ext' ∘ prop (Proxy ∷ _ "nbBases")
_missingPeg ∷ Lens' Model Int
_missingPeg = _ext' ∘ prop (Proxy ∷ _ "missingPeg")

-- | état initial
imodel ∷ Model
imodel =
  genModel [] identity (Ext
    {   nbBases: 5
    ,   missingPeg: 0
    }
  )

instance Game Position Ext Move where
  name _ = "baseball"

  play model i = do
    let position = model ^. _position
    let nbBases = model ^. _nbBases
    let j = model ^. _missingPeg
    x ← position !! i
    y ← position !! j
    guard $ elem (x / 2 - y / 2) [1, nbBases-1, -1, 1-nbBases]
    Just $ position # updateAtIndices [i ∧ y, j ∧ x]

  initialPosition model = shuffle $ 0 .. (2 * model^._nbBases - 1)
  isLevelFinished = view _position >>> allWithIndex \i j → i / 2 == j / 2
  onNewGame model = chooseInt' (2 * model^._nbBases) <#> \i → set _missingPeg i model
    
  -- fonctions par défault
  computerMove _ = pure Nothing
  sizeLimit = defaultSizeLimit
  updateScore s = defaultUpdateScore s
  onPositionChange = identity
  saveToJson _ = Nothing
  loadFromJson model _ = model

data Msg = Core CoreMsg | SetNbBases Int | Play Move
instance MsgWithCore Msg where core = Core

update ∷ Msg → UpdateMam Model Unit
update (Core msg) = coreUpdate msg
update (SetNbBases n) = newGame $ set _nbBases n
update (Play m) = playA m
