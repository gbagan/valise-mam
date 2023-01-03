module Game.Frog.Model where

import MamPrelude
import Data.Lazy (defer, force)
import Lib.KonamiCode (konamiCode)
import Lib.Update (UpdateMam)
import Game.Core (class Game, class TwoPlayersGame, class MsgWithCore, CoreMsg, Mode(..), GState, SizeLimit(..),
              playA,  _ext, coreUpdate, newGame, computerMove', genState, _position, _nbRows, defaultUpdateScore)

-- une position est la case sur laquelle se trouve de la grenouille
-- les positions vont de 0 à nbRows
--      nbRows est la position initiale et 0 la position finale
-- un coup (move) est la case sur laquelle on veut déplacer la grenouille

type Ext' =
    {   moves ∷ Array Int  -- la liste des mouvements autorisées (en nombre de cases)
    ,   winning ∷ Array Boolean      -- la liste des positions gagnantes
    ,   marked ∷ Array Boolean       -- la liste des posiions marquées par l'utilisateur
    ,   keySequence ∷ Array String   -- pour le konami code
    }
newtype ExtState = Ext Ext'

type State = GState Int ExtState

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_moves ∷ Lens' State (Array Int)
_moves = _ext' ∘ prop (Proxy ∷ _ "moves")
_winning ∷ Lens' State (Array Boolean)
_winning = _ext' ∘ prop (Proxy ∷ _ "winning")
_marked ∷ Lens' State (Array Boolean)
_marked = _ext' ∘ prop (Proxy ∷ _ "marked")
_keySequence ∷ Lens' State (Array String)
_keySequence = _ext' ∘ prop (Proxy ∷ _ "keySequence")

-- | état initial
istate ∷ State
istate = genState
            20   -- position 
            _{  nbRows = 20
            ,   mode = RandomMode
            ,   customSize = true
            } (Ext
            {   moves: [1, 2, 3]
            ,   winning: []
            ,   marked: []
            ,   keySequence: []
            })

canPlay ∷ State → Int → Boolean
canPlay state v = elem (position - v) moves || position > 0 && v == 0 && position <= maximum where
    position = state^._position
    moves = state^._moves
    maximum = foldr max 0 moves

-- | renvoie l'ensemble des positions gagnantes pour une taille et un ensemble de mouvements donnés
winningPositions ∷ Int → Array Int → Array Boolean
winningPositions size moves = t <#> force where
    t = repeat size \i → defer
            \_ → i == 0 || (moves # all \m → maybe false (not ∘ force) (t !! (i - m)))

-- | renvoie les positions accessibles depluis la position courante
reachableArray ∷ State → Array Boolean
reachableArray state = repeat (state^._nbRows + 1) (canPlay state)

instance Game Int ExtState Int where
    name _ = "frog"
    play state v = if canPlay state v then Just v else Nothing
    initialPosition state = pure $ state^._nbRows
    onNewGame state = pure $ state
                            # set _winning (winningPositions (state^._nbRows + 1) (state^._moves))
                            # set _marked (replicate (state^._nbRows + 1) false)
    isLevelFinished state = state^._position == 0
    computerMove st = computerMove' st
    sizeLimit _ = SizeLimit 5 0 30 0

    onPositionChange = identity
    updateScore s = defaultUpdateScore s
    saveToJson _ = Nothing
    loadFromJson st _ = st

instance TwoPlayersGame Int ExtState Int where
    possibleMoves state = filter (canPlay state) (0 .. (state^._nbRows))
    isLosingPosition state = state^._winning !! (state^._position) ?: true

data Msg = Core CoreMsg | SelectMove Int | Mark Int | Play Int | Konami String
instance MsgWithCore Msg where core = Core

update ∷ Msg → UpdateMam State Unit
update (Core msg) = coreUpdate msg
-- ajoute ou enlève un mouvement dans la liste des mouvements permis
update (SelectMove move) = newGame $ over _moves selectMove where
    selectMove moves =
        let moves' = 1 .. 5 # filter (\m → (m == move) ≠ elem m moves) in
        if null moves' then moves else moves' 
        -- place/retire une marque à la position i
update (Mark i) = _marked ∘ ix i %= not
update (Play i) = playA i
update (Konami s) = s # konamiCode _keySequence (modify_ \st → st # _marked .~ st^._winning)

onKeyDown ∷ String → Maybe Msg
onKeyDown = Just ∘ Konami 
