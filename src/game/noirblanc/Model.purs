module Game.Noirblanc.Model where

import MamPrelude
import Control.Monad.Gen (chooseInt)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Game.Core (class MsgWithCore, class Game, GModel, SizeLimit(..), CoreMsg,
                  _ext, coreUpdate, playA, isLevelFinished, saveToStorage,
                  _position, _nbColumns, _nbRows, _customSize, newGame, genModel, defaultUpdateScore)
import Lib.KonamiCode (konamiCode)
import Lib.Update (UpdateMam)
import Lib.Util (chooseInt', dCoords)

-- une position est composée de 2 tableaux light et played
-- light indique si la case de numéro i est allumée
-- played indique si l'on a joué sur la case de numéro i
-- un coup (move) est représenté par le numéro de case que l'on active
type Position = { light ∷ Array Boolean, played ∷ Array Boolean }

type Ext' = 
    {   mode ∷ Int                 -- entre 0 et 3
    ,   level ∷ Int                -- le niveau en cours
    ,   maxLevels ∷ Array Int      -- pour chaque mode, le nombre de niveaux débloqués
    ,   keySequence ∷ Array String -- pour le konami code
    }

newtype ExtModel = Ext Ext'
type Model = GModel Position ExtModel

-- état initial
imodel ∷ Model
imodel = genModel {light: [], played: []} identity 
        (Ext 
            {   level: 0
            ,   mode: 0
            ,   maxLevels: [0, 1, 1, 0]
            ,   keySequence: []
            }
        )

-- lenses
_ext' ∷ Lens' Model Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_mode ∷ Lens' Model Int
_mode = _ext' ∘ prop (Proxy ∷ _ "mode")
_level ∷ Lens' Model Int
_level = _ext' ∘ prop (Proxy ∷ _ "level")
_maxLevels ∷ Lens' Model (Array Int)
_maxLevels = _ext' ∘ prop (Proxy ∷ _ "maxLevels")
_keySequence ∷ Lens' Model (Array String)
_keySequence = _ext' ∘ prop (Proxy ∷ _ "keySequence")
_light ∷ Lens' Position (Array Boolean)
_light = prop (Proxy ∷ _ "light")
_played ∷ Lens' Position (Array Boolean)
_played = prop (Proxy ∷ _ "played")

-- | indique si index1 est voisine de index2 selon un mode de jeu donné
-- | c'est à dire que si l'on active index1, index2 va changer de couleur
neighbor ∷ Model → Int → Int → Boolean
neighbor model index1 index2 =
    row * row + col * col == 1
    || mode `mod` 3 == 0 && index1 == index2 
    || mode >= 2 && index1 /= index2 && row * col == 0
    where
        mode = model^._mode
        {row, col} = dCoords (model^._nbColumns) index1 index2
    
-- | met à jour le tableau light en fonction du coup joué à la position index
toggleCell ∷ Model → Int → Array Boolean → Array Boolean
toggleCell model index = mapWithIndex \i → (_ ≠ neighbor model index i)

-- | génération de plateau aléatoire pour le niveau 6
-- | on part d'une configuration où tout est éteint et on joue des coups aléatoires
genRandomBoard ∷ Model → Gen (Array Boolean)
genRandomBoard model = do
    let size = model^._nbRows * model^._nbColumns
    nbMoves ← chooseInt size (size+1)
    moves ∷ Array Int ← replicateA nbMoves (chooseInt' size)
    pure $ foldr (toggleCell model) (replicate size false) moves

instance Game Position ExtModel Int where
    name _ = "noirblanc"

    play model index = Just $ model^._position 
                        # _light %~ (toggleCell model index)
                        # _played ∘ ix index %~ not

    initialPosition model = do
        let size = model^._nbRows * model^._nbColumns
        board ← if model^._level >= 6 then genRandomBoard model else pure (replicate size true)
        pure { light: board, played: replicate size false }
    
    isLevelFinished model = all not (model^._position).light

    onNewGame model = 
        let rows ∧ columns = sizes !! (model^._level) ?: 8∧8 in
        pure $
            if model^._level < 5 then
                model # _customSize .~ false
                      # _nbRows .~ rows 
                      # _nbColumns .~ columns
            else if not (model^._customSize) then 
                model # _customSize .~ true
                      # _nbRows .~ 8
                      # _nbColumns .~ 8
            else
                model

    sizeLimit _ = SizeLimit 2 2 12 12

    saveToJson model = Just $ encodeJson $ model^._maxLevels
    loadFromJson model json =
        case decodeJson json of
            Left _ → model
            Right maxLevels → model # _maxLevels .~ maxLevels 

    -- méthodes par default
    computerMove _ = pure Nothing
    updateScore s = defaultUpdateScore s
    onPositionChange = identity

sizes ∷ Array (Tuple Int Int)
sizes = [3∧3, 4∧4, 2∧10, 3∧10, 5∧5, 8∧8, 8∧8]

-- | si le niveau est fini, on met à jour les nivaux débloqués
-- | et l'on passe au niveau suivant
afterPlay ∷ UpdateMam Model Msg Unit
afterPlay = do
    model ← get
    let mode = model^._mode
    when (isLevelFinished model) do
        let nextLevel = if model^._level >= 4 then
                        6
                    else
                        model^._level + (if mode == 0 || mode == 3 then 1 else 2)
        _maxLevels ∘ ix mode .= nextLevel
        saveToStorage
        newGame $ over _level \n → min (n + 1) 6

data Msg = Core CoreMsg | SelectMode Int | SelectLevel Int | Play Int | Konami String
instance MsgWithCore Msg where core = Core

update ∷ Msg → UpdateMam Model Msg Unit
update (Core msg) = coreUpdate msg
update (SelectMode mode) = newGame $ set _mode mode ∘ set _level 0
update (SelectLevel level) = newGame $ set _level level
update (Play move) = playA move *> afterPlay
update (Konami k) = konamiCode _keySequence (_maxLevels .= [6, 6, 6, 6]) k

onKeyDown ∷ String → Maybe Msg
onKeyDown = Just ∘ Konami