module Game.Frog.Model where

import MyPrelude
import Data.Lazy (defer, force)
import Lib.Util (tabulate, (..))
import Data.Array.NonEmpty (NonEmptyArray, singleton, fromArray, cons) as N
import Lib.KonamiCode (konamiCode)
import Pha.Action (Action, RNG, DELAY, setState, delay)
import Game.Core (class Game, class TwoPlayersGame, Mode(..), GState, SizeLimit(..),
                _ext, playA, lockAction, newGame', computerMove', genState, _position, _nbRows)

type Ext' = {
    moves :: N.NonEmptyArray Int,  -- la liste des mouvements autorisées (en nombre de cases)
    winning :: Array Boolean, --- la liste des positions gagnantes
    marked :: Array Boolean,  -- la liste des posiions marquées par l'utilisateur
    keySequence :: Array String    --- pour le konami code
}
newtype ExtState = Ext Ext'

type State = GState Int ExtState

-- lenses
_ext' :: Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) -> a) Ext
_moves :: Lens' State (N.NonEmptyArray Int)
_moves = _ext' ∘ lens _.moves _{moves = _}
_winning :: Lens' State (Array Boolean)
_winning = _ext' ∘ lens _.winning _{winning = _}
_marked :: Lens' State (Array Boolean)
_marked = _ext' ∘ lens _.marked _{marked = _}
_keySequence :: Lens' State (Array String)
_keySequence = _ext' ∘ lens _.keySequence _{keySequence = _}

-- état initial
istate :: State
istate = genState 20 _{nbRows = 20, mode = ExpertMode, customSize = true} (Ext { moves: 1 `N.cons` (2 `N.cons` N.singleton 3),
                                                            winning: [], marked: [], keySequence: [] })

canPlay :: State -> Int -> Boolean
canPlay state v = elem (position - v) moves || position > 0 && v == 0 && position <= maximum where
    position = state^._position
    moves = state^._moves
    maximum = foldr max 0 moves

instance frogGame :: Game Int ExtState Int where
    play state v = if canPlay state v then Just v else Nothing
    initialPosition state = pure $ state^._nbRows
    onNewGame state = pure $ state
                        # _winning .~ winningPositions (state^._nbRows + 1) (state^._moves)
                        # _marked .~ replicate (state^._nbRows + 1) false
    isLevelFinished state = state^._position == 0
    computerMove = computerMove'
    sizeLimit _ = SizeLimit 5 0 30 0
    updateScore st = st ∧ true

instance frogGame2 :: TwoPlayersGame Int ExtState Int where
    possibleMoves state = filter (canPlay state) (0 .. (state^._nbRows))
    isLosingPosition state = fromMaybe true $ state^._winning !! (state^._position)


-- calcule l'ensemble des positions gagnantes pour une taille et un ensemble de mouvements donnés
winningPositions :: ∀t. Foldable t => Int -> t Int -> Array Boolean
winningPositions size moves = t <#> force where
    t = tabulate size \i -> defer
            \_ -> i == 0 || (moves # all \m -> maybe false (not ∘ force) (t !! (i - m)))

--- calcule les positions accessibles depluis la position courante
reachableArray :: State -> Array Boolean
reachableArray state = tabulate (state^._nbRows + 1) (canPlay state)

-- ajoute ou enlève un mouvement dans la liste des mouvements permis
selectMoveA :: ∀effs. Int -> Action State (rng :: RNG | effs)
selectMoveA = newGame' $ over _moves ∘ selectMove where
    selectMove move moves =
        1 .. 5 
        # filter (\m -> (m == move) /= elem m moves)
        # N.fromArray
        # fromMaybe moves

-- ajoute une pause de 500ms après un mouvement pour retarder l'affichage des positions accessibles
playA' :: ∀effs. Int -> Action State (delay :: DELAY, rng :: RNG | effs)
playA' i = playA i *> lockAction (delay 500)

-- place/retire une marque à la position i
markA :: ∀effs. Int -> Action State effs
markA i = setState (_marked ∘ ix i %~ not)

onKeyDown :: ∀effs. String -> Action State effs
onKeyDown = konamiCode _keySequence (setState \st -> st # _marked .~ st^._winning)

