module Game.Frog.Model where

import Prelude
import Data.Array ((..), (!!), elem, foldr, filter, all, null, replicate)
import Data.Lazy (defer, force)
import Data.Maybe (maybe, fromMaybe)
import Data.Lens (Lens', lens, (^.), (.~), (%~), over)
import Data.Lens.Index (ix)
import Lib.Core (tabulate)
import Pha (Action, action)
import Game.Core (class Game, canPlay, class TwoPlayersGame, Mode(..), State(..), SizeLimit(..),
                newGame', computerMove', genState, _position, _nbRows)

type Position = Int

type Ext' = {
    moves :: Array Int,
    winning :: Array Boolean,
    marked :: Array Boolean
}
newtype ExtState = Ext Ext'

type FrogState = State Position ExtState

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
        --            help: false,
        --            hideReachable: false,
        --        }),
    isLevelFinished state = state^._position == 0
    computerMove = computerMove'
    sizeLimit _ = SizeLimit 5 0 30 0

instance frogGame2 :: TwoPlayersGame Int ExtState Int where
    possibleMoves state = filter (canPlay state) (0 .. (state^._nbRows))
    isLosingPosition state = fromMaybe true $ state^._winning !! (state^._position)

selectMoveA :: Int -> Action FrogState
selectMoveA = newGame' $ over _moves <<< _selectMove where
    _selectMove move moves =
        let moves2 = filter (\m -> (m == move) /= elem m moves) (1 .. 5) in
        if null moves2 then moves else moves2

winningPositions :: Int -> Array Int -> Array Boolean
winningPositions size moves =
    let t = tabulate size \i -> defer
            \_ -> i == 0 || (moves # all \m -> maybe false (not <<< force) (t !! (i - m))) in
    t <#> force

reachableArray :: FrogState -> Array Boolean
reachableArray state = tabulate (state^._nbRows + 1) (canPlay state)

markA :: Int -> Action FrogState
markA i = action $ (_marked <<< ix i) %~ not

-- export default template({
--    state: {
--        moves: [1, 2, 3],
--        mode: 'expert',
--        keysequence: [],
--        rows: 20,
--        columns: 1,
--        customSize: true,
--    },

--   core: {
--        sizeLimit: [5, 1, 20, 1],
--        play,
--        canPlay,
--        newGame: state => ({
--            winning: winningPositions(state.rows + 1, state.moves),
--            marked: tabulate(state.rows + 1 , false),
--            help: false,
--            hideReachable: false,
--        }),
--    },

--    actions: $ => ({
--        mark: update(i => set(['marked', i], not)),

--        
--        play: when(canPlay, combine($.play, s => asyncToggle('hideReachable', s.mode === 'duel' ? 500 : 1500)(s))),
--        toggleHelp: set('help', not),
--        keyDown: konamiCode(state => ({...state, marked: state.winning})),
--    }),

--   computed: state => ({
--     
--    }),
--});