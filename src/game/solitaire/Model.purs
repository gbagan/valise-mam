module Game.Solitaire.Model where
import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (sequence)
import Data.Array ((!!), replicate, all, mapWithIndex, updateAtIndices)
import Data.Lens (Lens', lens, view, (^.), (.~))
import Data.Lens.Index (ix)
import Lib.Random (Random, randomBool)
import Lib.Core (tabulate, tabulate2, dCoords)
import Game.Core (class Game, State(..), SizeLimit(..), genState, canPlay, _nbColumns, _nbRows, _position)

type Move = {from :: Int, to :: Int}

data Board = FrenchBoard | EnglishBoard | CircleBoard | Grid3Board | RandomBoard
derive instance boardMode :: Eq Board

type Ext' = {
    board :: Board, holes :: Array Boolean, dragged :: Maybe Int
}

newtype ExtState = Ext Ext'
type SolitaireState = State (Array Boolean) ExtState

solitaireState :: SolitaireState
solitaireState = genState [] (_{nbRows = 5, nbColumns = 1}) (Ext { board: CircleBoard, holes: [], dragged: Nothing })

_ext :: Lens' SolitaireState Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_board :: Lens' SolitaireState Board
_board = _ext <<< lens (_.board) (_{board = _})
_holes :: Lens' SolitaireState (Array Boolean)
_holes = _ext <<< lens (_.holes) (_{holes = _})
_dragged :: Lens' SolitaireState (Maybe Int)
_dragged = _ext <<< lens (_.dragged) (_{dragged = _})

betweenMove :: SolitaireState -> Move -> Maybe Int
betweenMove state { from, to } = 
    let {row, col} = dCoords (state^._nbColumns) from to in
    if row * row + col * col == 4 then Just $ (from + to) / 2 else Nothing

betweenInCircle :: Int -> Int -> Int -> Maybe Int
betweenInCircle from to size =
    if from - to == 2 || to - from == 2 then
        Just $ (from + to) / 2
    else if (to - from) `mod` size == 2 then
        Just $ (from + 1) `mod` size
    else if (from - to) `mod` size == 2 then
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

generateBoard :: Int -> Int -> Int -> ({row :: Int, col :: Int} -> Boolean) -> {holes :: Array Boolean, position :: Random (Array Boolean)}
generateBoard rows columns startingHole holeFilter = {holes, position} where
    holes = tabulate2 rows columns holeFilter
    position = pure $ holes # ix startingHole .~ false

instance solitaireGame :: Game (Array Boolean) ExtState {from :: Int, to :: Int} where
    canPlay state move@{from, to} = fromMaybe false $ do
        let position = state^._position
        between <- betweenMove2 state move
        pfrom <- position !! from
        pbetween <- position !! between
        pto <- position !! to
        hto <- state^._holes !! to
        pure $ pfrom && pbetween && hto && not pto

    play state move@{from, to} = maybe (state^._position)
        (\between -> state^._position # updateAtIndices [Tuple from false, Tuple between false, Tuple to true])
        (betweenMove2 state move)

    initialPosition = pure <<< view _position

    isLevelFinished state =
        all identity $ state^._position # mapWithIndex \i val ->
            ([2, -2, 2 * state^._nbColumns, -2 * state^._nbColumns, state^._nbRows - 2] # all \d ->
                not canPlay state { from: i, to: i + d }
            )

    onNewGame state = position <#> \p -> state # _holes .~ holes # _position .~ p where
        columns = state^._nbColumns
        {holes, position} =
            case state^._board of
                EnglishBoard -> generateBoard 7 7 24 \{row, col} -> min row (6 - row) >= 2 || min col (6 - col) >= 2
                FrenchBoard -> generateBoard 7 7 24 \{row, col} -> min row (6 - row) + min col (6 - col) >= 2
                CircleBoard -> generateBoard (state^._nbRows) 1 0 \_ -> true
                Grid3Board -> {
                    holes: replicate (3 * state^._nbColumns) true,
                    position: pure $ tabulate (3 * state^._nbColumns) (_ <= 2 * columns)
                }
                RandomBoard -> {
                    holes: replicate (3 * state^._nbColumns) true,
                    position: (sequence $ replicate columns randomBool) <#> \bools -> bools <> replicate columns true <> bools <#> not
                }

    sizeLimit state = if state^._board == CircleBoard then SizeLimit 3 1 12 1 else SizeLimit 3 1 3 9

    computerMove _ = Nothing

{-
const dimensions = {
    french: { rows: 7, columns: 7, customSize: false },
    english: { rows: 7, columns: 7, customSize: false },
    circle: { rows: 6, columns: 1, customSize: true },
    grid: { rows: 3, columns: 5, customSize: true },
    random: { rows: 3, columns: 5, customSize: true },
};

{-

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