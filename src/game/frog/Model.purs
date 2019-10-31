module Game.Frog.Model where

import Prelude
import Data.Array ((..), (!!), elem, foldr, filter, all, null, replicate)
import Data.Lazy (defer, force)
import Data.Maybe (maybe, fromMaybe)
import Data.Lens (Lens', lens, (^.), (.~), (%~), over)
import Data.Lens.Index (ix)
import Lib.Util (tabulate)
import Pha.Action (Action, action)
import Game.Core (class Game, canPlay, class TwoPlayersGame, Mode(..), State(..), SizeLimit(..),
                newGame', computerMove', genState, _position, _nbRows)

type Ext' = {
    moves :: Array Int,  -- la liste des mouvements autorisées (en nombre de cases)
    winning :: Array Boolean, --- la liste des positions gagnantes
    marked :: Array Boolean  -- la liste des posiions marquées par l'utilisateur 
}
newtype ExtState = Ext Ext'

type FrogState = State Int ExtState

-- lenses
_ext :: Lens' FrogState Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_moves :: Lens' FrogState (Array Int)
_moves = _ext <<< lens (_.moves) (_{moves = _})
_winning :: Lens' FrogState (Array Boolean)
_winning = _ext <<< lens (_.winning) (_{winning = _})
_marked :: Lens' FrogState (Array Boolean)
_marked = _ext <<< lens (_.marked) (_{marked = _})

frogState :: FrogState
frogState = genState 20 (_{nbRows = 20, mode = ExpertMode}) (Ext { moves: [1, 2, 3], winning: [], marked: [] })

instance frogGame :: Game Int ExtState Int where
    play state v = v
    canPlay state v = elem (position - v) moves || position > 0 && v == 0 && position <= maximum where
        position = state^._position
        moves = state^._moves
        maximum = foldr max 0 moves

    initialPosition state = pure $ state^._nbRows
    onNewGame state = pure $ state
                        # _winning .~ winningPositions (state^._nbRows + 1) (state^._moves)
                        # _marked .~ replicate (state^._nbRows + 1) false
    isLevelFinished state = state^._position == 0
    computerMove = computerMove'
    sizeLimit _ = SizeLimit 5 0 30 0

instance frogGame2 :: TwoPlayersGame Int ExtState Int where
    possibleMoves state = filter (canPlay state) (0 .. (state^._nbRows))
    isLosingPosition state = fromMaybe true $ state^._winning !! (state^._position)

-- ajoute ou enlève un mouvement dans la liste des mouvements permis
selectMoveA :: Int -> Action FrogState
selectMoveA = newGame' $ over _moves <<< _selectMove where
    _selectMove move moves =
        let moves2 = filter (\m -> (m == move) /= elem m moves) (1 .. 5) in
        if null moves2 then moves else moves2

-- calcule l'ensemble des positions gagnantes pour une taille et un ensemble de mouvements donnés
winningPositions :: Int -> Array Int -> Array Boolean
winningPositions size moves =
    let t = tabulate size \i -> defer
            \_ -> i == 0 || (moves # all \m -> maybe false (not <<< force) (t !! (i - m))) in
    t <#> force

--- calcule les positions accessibles depluis la position courante
reachableArray :: FrogState -> Array Boolean
reachableArray state = tabulate (state^._nbRows + 1) (canPlay state)

-- place/retire une marque à la position i
markA :: Int -> Action FrogState
markA i = action $ (_marked <<< ix i) %~ not

--        keyDown: konamiCode(state => ({...state, marked: state.winning})),
--    }),
