module Game.Frog.Model where

type Position = Int
type Move = Int
data ExtState = Ext {
    moves :: Array Int
}
type FrogState = State Position ExtState

_moves :: Lens' FrogState (Array Int)
_moves = lens (\(State _ (Ext s)) -> s.moves) (\(State s (Ext ext)) x -> State s (Ext ext{moves = x}))

example :: FrogState
example = genState [] identity (Ext { moves: [1, 2, 3] })

instance frogGame :: Game Int ExtState Int where
    play state v = v
    canPlay state v = elem (position - v) moves || position /= 0 && v == 0 && position <= maximum moves
        where position = state^._position
              moves = state._moves
    initialPosition state = pure $ state^._rows
    isLevelFinished state v = v == 0
        

toggleMove :: Array Int -> Int -> Array Int
toggleMove moves move = filter (\m -> (m == move) /= elem m moves) (1 .. 5) <|> moves

instance frogGame' :: TwoPlayersGame Int ExtState Int where
    possibleMoves state = filter (canPlay state) (0 .. 21)
    isLostPosition state = fromMaybe 0 $ state^._winning !! state^._position

winningPositions :: Int -> Array Int -> Array Boolean
winningPositions size moves =
    let  t = unsaferepeat size \i -> defer
        \_ -> i === 0 || all \m -> fromMaybe false $ force $ unsafeIndex t i in
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
--            marked: repeat(state.rows + 1 , false),
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
--     reachable: repeat(state.rows + 1, i => canPlay(state, i)),
--    }),
--});