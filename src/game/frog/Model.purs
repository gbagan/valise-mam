module Game.Frog.Model where

import Prelude
import Data.Array ((..), (!!), elem, foldr, filter, all)
import Data.Lazy (defer, force)
import Data.Maybe (maybe, fromMaybe)
import Control.Alt ((<|>))
import Optic.Core (Lens', lens, (^.))
import Lib.Core (tabulate)
import Lib.Game (class Game, canPlay, class TwoPlayersGame, State(..), computerMove', genState, _position, _nbRows)

type Position = Int

data ExtState = Ext {
    moves :: Array Int,
    winning :: Array Boolean
}

type FrogState = State Position ExtState

_moves :: Lens' FrogState (Array Int)
_moves = lens (\(State _ (Ext s)) -> s.moves) (\(State s (Ext ext)) x -> State s (Ext ext{moves = x}))
_winning :: Lens' FrogState (Array Boolean)
_winning = lens (\(State _ (Ext s)) -> s.winning) (\(State s (Ext ext)) x -> State s (Ext ext{winning = x}))

example :: FrogState
example = genState 20 identity (Ext { moves: [1, 2, 3], winning: [] })

instance frogGame :: Game Int ExtState Int where
    play state v = v
    canPlay state v = elem (position - v) moves || position /= 0 && v == 0 && position <= maximum where
        position = state^._position
        moves = state^._moves
        maximum = foldr max 0 moves

    initialPosition state = pure $ state^._nbRows
    isLevelFinished state = state^._position == 0
    computerMove = computerMove'

instance frogGame2 :: TwoPlayersGame Int ExtState Int where
    possibleMoves state = filter (canPlay state) (0 .. 20)
    isLosingPosition state = fromMaybe true $ state^._winning !! (state^._position)

-- toggleMove :: Array Int -> Int -> Array Int
-- toggleMove moves move = filter (\m -> (m == move) /= elem m moves) (1 .. 5) <|> moves

winningPositions :: Int -> Array Int -> Array Boolean
winningPositions size moves =
    let t = tabulate size \i -> defer
            \_ -> i == 0 || (moves # all \m -> maybe false force (t !! (i - m))) in
    t <#> force

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

--        selectMove: $.newGame(move => state =>
--            ({...state, moves: toggleMove(state.moves, move)})
--        ),
--        play: when(canPlay, combine($.play, s => asyncToggle('hideReachable', s.mode === 'duel' ? 500 : 1500)(s))),
--        toggleHelp: set('help', not),
--        keyDown: konamiCode(state => ({...state, marked: state.winning})),
--    }),

--   computed: state => ({
--     reachable: tabulate(state.rows + 1, i => canPlay(state, i)),
--    }),
--});