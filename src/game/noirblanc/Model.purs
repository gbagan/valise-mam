module Game.Noirblanc.Model where

import MyPrelude
import Control.Monad.Gen (chooseInt)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Game.Core (class MsgWithCore, class Game, GState, SizeLimit(..), CoreMsg, _ext, coreUpdate, playA, isLevelFinished, saveToStorage,
                  _position, _nbColumns, _nbRows, _customSize, newGame, genState)
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

newtype ExtState = Ext Ext'
type State = GState Position ExtState

-- état initial
istate ∷ State
istate = genState {light: [], played: []} identity 
        (Ext 
            {   level: 0
            ,   mode: 0
            ,   maxLevels: [0, 1, 1, 0]
            ,   keySequence: []
            }
        )

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_mode ∷ Lens' State Int
_mode = _ext' ∘ prop (Proxy ∷ _ "mode")
_level ∷ Lens' State Int
_level = _ext' ∘ prop (Proxy ∷ _ "level")
_maxLevels ∷ Lens' State (Array Int)
_maxLevels = _ext' ∘ prop (Proxy ∷ _ "maxLevels")
_keySequence ∷ Lens' State (Array String)
_keySequence = _ext' ∘ prop (Proxy ∷ _ "keySequence")
_light ∷ Lens' Position (Array Boolean)
_light = prop (Proxy ∷ _ "light")
_played ∷ Lens' Position (Array Boolean)
_played = prop (Proxy ∷ _ "played")

-- | indique si index1 est voisine de index2 selon un mode de jeu donné
-- | c'est à dire que si l'on active index1, index2 va changer de couleur
neighbor ∷ State → Int → Int → Boolean
neighbor state index1 index2 =
    row * row + col * col == 1
    || mode `mod` 3 == 0 && index1 == index2 
    || mode >= 2 && index1 ≠ index2 && row * col == 0
    where
        mode = state^._mode
        {row, col} = dCoords (state^._nbColumns) index1 index2
    
-- | met à jour le tableau light en fonction du coup joué à la position index
toggleCell ∷ State → Int → Array Boolean → Array Boolean
toggleCell state index = mapWithIndex \i → (_ ≠ neighbor state index i)

-- | génération de plateau aléatoire pour le niveau 6
-- | on part d'une configuration où tout est éteint et on joue des coups aléatoires
genRandomBoard ∷ ∀m. MonadGen m ⇒ State → m (Array Boolean)
genRandomBoard state = do
    let size = state^._nbRows * state^._nbColumns
    nbMoves ← chooseInt size (size+1)
    moves ∷ Array Int ← replicateA nbMoves (chooseInt' size)
    pure $ foldr (toggleCell state) (replicate size false) moves

instance Game Position ExtState Int where
    name _ = "noirblanc"

    play state index = Just $ state^._position 
                        # _light %~ (toggleCell state index)
                        # _played ∘ ix index %~ not

    initialPosition state = do
        let size = state^._nbRows * state^._nbColumns
        board ← if state^._level >= 6 then genRandomBoard state else pure (replicate size true)
        pure { light: board, played: replicate size false }
    
    isLevelFinished state = all not (state^._position).light

    onNewGame state = 
        let rows ∧ columns = sizes !! (state^._level) ?: 8∧8 in
        pure $
            if state^._level < 5 then
                state # _customSize .~ false
                      # _nbRows .~ rows 
                      # _nbColumns .~ columns
            else if not (state^._customSize) then 
                state # _customSize .~ true
                      # _nbRows .~ 8
                      # _nbColumns .~ 8
            else
                state

    sizeLimit _ = SizeLimit 2 2 12 12

    saveToJson st = Just $ encodeJson $ st ^. _maxLevels
    loadFromJson st json =
        case decodeJson json of
            Left _ → st
            Right maxLevels → st # _maxLevels .~ maxLevels 

    -- méthodes par default
    computerMove _ = pure Nothing
    updateScore st = st ∧ true
    onPositionChange = identity

sizes ∷ Array (Tuple Int Int)
sizes = [3∧3, 4∧4, 2∧10, 3∧10, 5∧5, 8∧8, 8∧8]

-- | si le niveau est fini, on met à jour les nivaux débloqués
-- | et l'on passe au niveau suivant
afterPlay ∷ UpdateMam State
afterPlay = do
    state ← get
    let mode = state^._mode
    when (isLevelFinished state) do
        let nextLevel = if state^._level >= 4 then
                        6
                    else
                        state^._level + (if mode == 0 || mode == 3 then 1 else 2)
        _maxLevels ∘ ix mode .= nextLevel
        saveToStorage
        newGame $ over _level \n → min (n + 1) 6

data Msg = Core CoreMsg | SelectMode Int | SelectLevel Int | Play Int | Konami String
instance MsgWithCore Msg where core = Core

update ∷ Msg → UpdateMam State
update (Core msg) = coreUpdate msg
update (SelectMode mode) = newGame $ set _mode mode ∘ set _level 0
update (SelectLevel level) = newGame $ set _level level
update (Play move) = playA move *> afterPlay
update (Konami k) = konamiCode _keySequence (_maxLevels .= [6, 6, 6, 6]) k

onKeyDown ∷ String → Maybe Msg
onKeyDown = Just ∘ Konami