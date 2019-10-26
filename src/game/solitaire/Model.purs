module Game.Solitaire.Model where
import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Array ((!!))
import Optic.Core (Lens', lens, (^.))
import Lib.Core (dCoords)
import Lib.Game (State(..), _nbColumns, _nbRows, _position)

type Move = {from :: Int, to :: Int}

data Board = CircleBoard | N3Board | RandomBord

derive instance boardMode :: Eq Board

data ExtState = Ext {
    board :: Board
}

type SolitaireState = State (Array Boolean) ExtState

_board :: Lens' SolitaireState Board
_board = lens (\(State _ (Ext s)) -> s.board) (\(State s (Ext ext)) x -> State s (Ext ext{board = x}))

betweenMove :: SolitaireState -> Move -> Maybe Int
betweenMove state { from, to } = 
    let {row, col} = dCoords (state^._nbColumns) from to in
    if row * row + col * col == 4 then Just $ (from + to) / 2 else Nothing

betweenInCircle :: Int -> Int -> Int -> Maybe Int
betweenInCircle from to size =
    if from - to == 2 || to - from == 2 then
        Just $ (from + to) / 2
    else if to - from `mod` size == 2 then
        Just $ (from + 1) `mod` size
    else if from - to `mod` size == 2 then
        Just $ (to + 1) `mod` size
    else
        Nothing
        
betweenMove2 :: SolitaireState -> Move -> Maybe Int
betweenMove2 state move@{from, to} =
    let rows = state ^._nbRows in
    if state^._board == CircleBoard then do
        x <- betweenInCircle from to rows
        pure $ if rows == 4 && maybe false not (state^._position !! x) then (x + 2) `mod` 4 else x
    else
        betweenMove state move

{-        
canPlay state {from, to}@move = fromMaybe false $ do
    between <- betweenMove2 state move
    pfrom <- state.position !! from
    pbetween <- state.position !! between
    pto <- state.position !! to
    hto <- state.holes !! to
    pure $ pfrom && pbetween && hto && not pto

    play state {from, to}@move = maybe state 
        (\between -> state.position # updateAtIndices [Tuple from false, Tuple between false, Tuple to true])
        (betweenMove2 state move)

    initialPosition state =
        if state.boardName == RandomBoard then do
            rbools <- sequence $ replicate state.columns randomBool
            rbools #
            repeat2(state.rows, state.columns, (row, col) => row === 1 || rints[col] === row / 2)
                )
        else
            pure state.position

    isLevelFinished state =
        state.position # mapWithIndex \i val ->
            ([2, -2, 2 * state.columns, -2 * state.columns, state.rows - 2] # all \d ->
                not canPlay state { from: i, to: i + d }
            ) # all identity    


const dimensions = {
    french: { rows: 7, columns: 7, customSize: false },
    english: { rows: 7, columns: 7, customSize: false },
    circle: { rows: 6, columns: 1, customSize: true },
    grid: { rows: 3, columns: 5, customSize: true },
    random: { rows: 3, columns: 5, customSize: true },
};

export default template({
    core: {
        name: 'solitaire',
        sizeLimit: state => state.boardName === 'circle' ? [3, 1, 12, 1] : [3, 1, 3, 9],

        newGame: state => boards[state.boardName](state),
    
    },

    state: {
        boardName: 'circle',
        rows: 6,
        columns: 1,
        customSize: true,
        help: 0,
    },

    actions: $ => ({
        setBoard: $.newGame(boardName => ({ boardName, ...dimensions[boardName] })),
        toggleHelp: set('help', i => (i + 1) % 3),
    }),

    computed: state => ({
        nbPegs: sum(state.position),
    })
});
-}