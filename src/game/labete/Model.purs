module Game.Labete.Model where
import MyPrelude
import Lib.Util (tabulate2)
import Pha.Action (Action)
import Game.Core (class Game, SizeLimit(..), GState(..),
                   genState, _position, _nbRows, _nbColumns)

type Beast = Array {row :: Int, col :: Int}
type1 :: Beast
type1 = [{ row: 0, col: 0 }, { row: 0, col: 1 }]
type2 :: Beast
type2 = [{ row: 0, col: 0 }, { row: 0, col: 1 }, { row: 0, col: -1 }]
type3 :: Beast
type3 = [{ row: 0, col: 0 }, { row: 0, col: 1 }, { row: 1, col: 1 }]
beastTypes :: Array (Array Beast)
beastTypes = [[type1], [type2], [type3], [type2, type3]]

{-
const fromCustomBeast = customBeast =>
    repeat2(5, 5, (row, col, i) => customBeast[i] ? {row: row - 2, col: col - 2} : null)
    |> filter(x => x !== null);

const zoneposition = ({ columns }, row1, col1, row2, col2) =>
    repeat2(Math.abs(row1 - row2) + 1, Math.abs(col1 - col2) + 1, (i, j) =>
        (i + Math.min(row1, row2)) * columns + j + Math.min(col1, col2)
    );


const colorZone = (state, zone) => zoneposition(state, zone.row1, zone.col1, zone.row2, zone.col2)
    |> reduce(pos => set(pos, state.selectedColor), state.squareColors);


const incSelectedColor = x => set('selectedColor', y => (x + y + 9) % 9);

const showBeast = state => state.help ? asyncToggle('beastVisible', 200)(state) : state;
-}


type Ext' = {
}
newtype ExtState = Ext Ext'
type State = GState (Array Boolean) ExtState

_ext :: Lens' State Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))

istate :: State
istate = genState [] (_{nbRows = 5, nbColumns = 5}) (Ext {})

rotate90 :: Beast -> Beast
rotate90 = map \{row, col} -> { row: -col, col: row }

translate :: Int -> Int -> Beast -> Beast
translate row' col' = map \{row, col} -> { row: row + row', col: col + col' }
        
allRotations :: Beast -> Array Beast
allRotations beast = [beast, beast2, beast3, beast4] where
    beast2 = rotate90 beast
    beast3 = rotate90 beast2
    beast4 = rotate90 beast3

allTranslations :: Int -> Int -> Beast -> Array Beast
allTranslations n m beast = tabulate2 n m \row col -> translate row col beast

allBeastPositions :: Int -> Int -> Array Beast -> Array Beast
allBeastPositions rows cols = concatMap $ allRotations >=> allTranslations rows cols

adaptatedConfig :: Int -> Int -> String -> Beast -> Beast
adaptatedConfig rows columns mode =
    map \{row, col} -> case mode of
                        "normal" -> {row, col}
                        "cylinder" -> {row, col: col `mod` columns}
                        _ -> {row: row `mod` rows, col: col `mod` columns}
        
-- const pseudoRandomPick = t => t[28921 % t.length];
{-
nonTrappedBeast = state =>
            allConfigs(state.rows, state.columns, state.beast)
            |> map(adaptedConfig(state.rows, state.columns, state.mode))
            |> filter(
                all(({row, col}) => row >= 0 && row < state.rows && col >= 0 && col < state.columns && !state.position[row * state.columns + col])
            )
            |> (s => s.length === 0 ?
                null
                : pseudoRandomPick(s)
                |> reduce(
                    p => set(p.row * state.columns + p.col, true),
                    repeat(state.rows * state.columns, false)
                )
            );
-}

instance labeteGame :: Game (Array Boolean) ExtState Int where
    play state index = state^._position # ix index %~ not
    canPlay _ _ = true
    isLevelFinished st = false --  not ∘ nonTrappedBeast
    initialPosition st = pure $ replicate (st^._nbRows * st^._nbColumns) false
    onNewGame st = pure st
    {- state => ({
        beast: beastTypes[state.beastIndex] || [fromCustomBeast(state.customBeast)],
        squareColors: duplicate(state.rows * state.columns, 0),
    -}
    sizeLimit _ = SizeLimit 2 2 9 9
    computerMove _ = Nothing
    {-
        score: {
            objective: 'minimize',
            function: state => sum(state.position),
            params: attrs('columns,rows,mode,beastIndex'),
            isCustomLevel: F,
        },
    -}
{-

    state: {
        columns: 5,
        rows: 5,
        beastIndex: 0,
        customBeast: duplicate(25, false) |> set(12, true) |> set(13, true),
        mode: 'normal', // normal | cylinder | torus

        customSize: false,
        help: false,
        beastVisible: false,

        squareHover: null,

        selectedColor: 0,
        zoneStart: null,
        zone: null,
    },

    actions: $ => ({
        setBeast: $.newGame(beastIndex => ({ beastIndex, dialog: beastIndex === 'custom' ? 'custombeast' : null })),
        setSquareHover: update('squareHover'),
        flipBeast: $.newGame(index => set(['customBeast', index], not)),

        startZone: (state, index) => state |> set('zoneStart', {index}),
        startZone2: state => state|> set('zoneStart', merge({
            left: 100 * state.pointerPosition.left / state.pointerPosition.width,
            top: 100 * state.pointerPosition.top / state.pointerPosition.height,
        })),

        play: combine($.play, showBeast),

        finishZone: (state, index) =>
            !state.zoneStart ? state : {
                ...state,
                squareColors: colorZone(state, {
                    row1: state.zoneStart.index / state.columns | 0,
                    col1: state.zoneStart.index % state.columns,
                    row2: index / state.columns | 0,
                    col2: index % state.columns
                }),
                zoneStart: null,
            },

        toggleHelp: pipe(set('help', not), showBeast),

        keyDown: keyEvents({
            o: incSelectedColor(-1),
            p: incSelectedColor(1),
        }),
    }),

    computed: state => ({
        nonTrappedBeast: nonTrappedBeast(state),
        zone: !state.zoneStart ? null : {
            x1: state.zoneStart.left,
            y1: state.zoneStart.top,
            x2: 100 * state.pointerPosition.left / state.pointerPosition.width,
            y2: 100 * state.pointerPosition.top / state.pointerPosition.height,
            color: state.selectedColor
        }
    })
});