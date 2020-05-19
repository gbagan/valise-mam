module Game.Noirblanc.Model where

import MyPrelude

import Game.Core (class MsgWithCore, class Game, GState, SizeLimit(..), CoreMsg, _ext, coreUpdate, playA, isLevelFinished, saveToStorage,
                _position, _nbColumns, _nbRows, newGame, genState)
import Game.Effs (EFFS, RNG, DELAY, STORAGE)
import Lib.KonamiCode (konamiCode)
import Lib.Util (dCoords)
import Pha.Random (Random)
import Pha.Random as R
import Pha.Update (Update, get, modify)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Decode (decodeJson)

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
_mode = _ext' ∘ lens _.mode _{mode = _}
_level ∷ Lens' State Int
_level = _ext' ∘ lens _.level _{level = _}
_maxLevels ∷ Lens' State (Array Int)
_maxLevels = _ext' ∘ lens _.maxLevels _{maxLevels = _}
_keySequence ∷ Lens' State (Array String)
_keySequence = _ext' ∘ lens _.keySequence _{keySequence = _}
_light ∷ Lens' Position (Array Boolean)
_light = lens _.light _{light = _}
_played ∷ Lens' Position (Array Boolean)
_played = lens _.played _{played = _}

-- | indique si index1 est voisine de index2 selon un mode de jeu donné
-- | c'est à dire que si l'on active index1, index2 va changer de couleur
neighbor ∷ State → Int → Int → Boolean
neighbor state index1 index2 =
    row * row + col * col == 1
    || mode `mod` 3 == 0 && index1 == index2 
    || mode >= 2 && index1 /= index2 && row * col == 0
    where
        mode = state^._mode
        {row, col} = dCoords (state^._nbColumns) index1 index2
    
-- | met à jour le tableau light en fonction du coup joué à la position index
toggleCell ∷ State → Int → Array Boolean → Array Boolean
toggleCell state index = mapWithIndex \i → (_ /= neighbor state index i)

-- | génération de plateau aléatoire pour le niveau 6
-- | on part d'une configuration où tout est éteint et on joue des coups aléatoires
genRandomBoard ∷ State → Random (Array Boolean)
genRandomBoard state = do
    let size = state^._nbRows * state^._nbColumns
    nbMoves ← R.int 0 size
    replicateA nbMoves (R.int' size) <#>
        foldr (toggleCell state) (replicate size true)

instance game ∷ Game { light ∷ Array Boolean, played ∷ Array Boolean } ExtState Int where
    name _ = "noirblanc"

    play state index = Just $ state^._position 
                        # _light %~ toggleCell state index 
                        # _played ∘ ix index %~ not

    initialPosition state = do
        let size = state^._nbRows * state^._nbColumns
        board ← if state^._level >= 6 then genRandomBoard state else pure (replicate size true)
        pure $ { light: board, played: replicate size false }
    
    isLevelFinished state = all not (state^._position).light

    onNewGame state = 
        let rows ∧ columns = fromMaybe (8∧8) (sizes !! (state^._level)) in
        pure (state # _nbRows .~ rows # _nbColumns .~ columns)

    sizeLimit _ = SizeLimit 3 3 10 10

    saveToJson st = Just $ encodeJson (st ^. _maxLevels)
    loadFromJson st json =
        case decodeJson json of
            Left _ -> st
            Right maxLevels -> st # _maxLevels .~ maxLevels 

    -- méthodes par default
    computerMove _ = pure Nothing
    updateScore st = st ∧ true
    onPositionChange = identity

sizes ∷ Array (Tuple Int Int)
sizes = [3∧3, 4∧4, 2∧10, 3∧10, 5∧5, 8∧8, 8∧8]

-- | si le niveau est fini, on met à jour les nivaux débloqués
-- | et l'on passe au niveau suivant
afterPlay ∷ ∀effs. Update State (rng ∷ RNG, delay ∷ DELAY, storage ∷ STORAGE | effs)
afterPlay = do
    state ← get
    let mode = state^._mode
    when (isLevelFinished state) do
        let nextLevel = if state^._level >= 4 then
                        6
                    else
                        state^._level + (if mode == 0 || mode == 3 then 1 else 2)
        modify $ _maxLevels ∘ ix mode .~ nextLevel
        saveToStorage
        newGame $ _level %~ \lvl → min (lvl + 1) 6

data Msg = Core CoreMsg | SelectMode Int | SelectLevel Int | Play Int | Konami String
instance withcore ∷ MsgWithCore Msg where core = Core

update ∷ Msg → Update State EFFS
update (Core msg) = coreUpdate msg
update (SelectMode mode) = newGame $ (_mode .~ mode) ∘ (_level .~ 0)
update (SelectLevel level) = newGame (_level .~ level)
update (Play move) = playA move *> afterPlay
update (Konami k) = konamiCode _keySequence (modify $ _maxLevels .~ [6, 6, 6, 6]) k

onKeyDown ∷ String → Maybe Msg
onKeyDown = Just <<< Konami