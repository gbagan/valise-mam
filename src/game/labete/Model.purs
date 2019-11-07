module Game.Labete.Model where
import MyPrelude
import Lib.Util (tabulate2)
import Data.Array (updateAtIndices)
import Pha.Action (Action, action, RNG)
import Game.Core (class Game, SizeLimit(..), GState(..),
                   genState, newGame', _position, _nbRows, _nbColumns, _help)

data Mode = StandardMode | CylinderMode | TorusMode
derive instance eqMode :: Eq Mode
instance showMode :: Show Mode where show _ = "mode"

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
-}


type Ext' = {
    beast :: Array Beast,
    beastIndex :: Int,
    mode :: Mode,
    selectedColor :: Int,
    squareColors :: Array Int
}
newtype ExtState = Ext Ext'
type State = GState (Array Boolean) ExtState

_ext :: Lens' State Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))

_beast :: Lens' State (Array Beast)
_beast = _ext ∘ lens (_.beast) (_{beast = _})

_beastIndex :: Lens' State Int
_beastIndex = _ext ∘ lens (_.beastIndex) (_{beastIndex = _})

_mode :: Lens' State Mode
_mode = _ext ∘ lens (_.mode) (_{mode = _})

_selectedColor :: Lens' State Int
_selectedColor = _ext ∘ lens (_.selectedColor) (_{selectedColor = _})

_squareColors :: Lens' State (Array Int)
_squareColors = _ext ∘ lens (_.squareColors) (_{squareColors = _})

istate :: State
istate = genState [] (_{nbRows = 5, nbColumns = 5}) (Ext {beast: [type1], beastIndex: 0, mode: StandardMode, 
                                                        squareColors: [], selectedColor: 0})

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

adaptatedBeast :: Int -> Int -> Mode -> Beast -> Beast
adaptatedBeast rows columns mode =
    map \{row, col} -> case mode of
                        StandardMode -> {row, col}
                        CylinderMode -> {row, col: col `mod` columns}
                        TorusMode -> {row: row `mod` rows, col: col `mod` columns}

pseudoRandomPick :: ∀t. Array t -> Maybe t
pseudoRandomPick t = t !! (28921 `mod` length t)


nonTrappedBeasts :: State -> Array Beast
nonTrappedBeasts state =
    allBeastPositions rows columns (state^._beast)
        <#> adaptatedBeast rows columns (state^._mode)
        # filter isValidBeast
    where rows = state^._nbRows
          columns = state^._nbColumns
          isValidBeast = all \{row, col} -> row >= 0 && row < rows && col >= 0 && col < columns && 
                    (state^._position) !! (row * columns + col) == Just false

nonTrappedBeastOnGrid :: State -> Array Boolean
nonTrappedBeastOnGrid st = 
    st # nonTrappedBeasts
    # pseudoRandomPick
    # fromMaybe []
    # foldr (\p -> ix (p.row * columns + p.col) .~ true) (replicate (rows * columns) false)
    where rows = st^._nbRows
          columns = st^._nbColumns

instance labeteGame :: Game (Array Boolean) ExtState Int where
    play state index = state^._position # ix index %~ not
    canPlay _ _ = true
    isLevelFinished = null ∘ nonTrappedBeasts
    initialPosition st = pure $ replicate (st^._nbRows * st^._nbColumns) false
    onNewGame st = pure $ st # _beast .~ (beastTypes !! (st^._beastIndex) # fromMaybe [type1])
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
setModeA :: ∀effs. Mode -> Action State (rng :: RNG | effs)
setModeA = newGame' (set _mode)

setHelpA :: ∀effs. Boolean -> Action State effs
setHelpA a = action (_help .~ a)

setBeastA :: ∀effs.  Int -> Action State (rng :: RNG | effs)
setBeastA = newGame' (set _beastIndex)

abs :: Int -> Int
abs x = if x < 0 then -x else x
          
type Zone = { row1 :: Int, row2 :: Int, col1 :: Int, col2 :: Int}

zoneposition :: Int -> Zone -> Array Int
zoneposition columns {row1, col1, row2, col2} =
    tabulate2 (abs (row1 - row2) + 1) (abs(col1 - col2) + 1) \i j ->
        i + (min row1 row2) * columns + j + (min col1 col2)
          
colorZone :: State -> Zone -> Array Int
colorZone state zone = state^._squareColors # updateAtIndices ( 
    zoneposition (state^._nbColumns) zone
    <#> \i -> Tuple i (state^._selectedColor)
)

incSelectedColorA :: ∀effs. Int -> Action State effs
incSelectedColorA x = action $ _selectedColor %~ \y -> (x + y + 9) `mod` 9

--startZone index action $ set _zoneStart %~ {index}),

--startZone state => set('zoneStart', merge({
--    left: 100 * state.pointer.left / state.pointer.width,
--    top: 100 * state.pointer.top / state.pointer.height,
--})),

    {-

    state: {
        beastIndex: 0,
        customBeast: duplicate(25, false) |> set(12, true) |> set(13, true),

        squareHover: null,

        zoneStart: null,
        zone: null,
    },

    actions: $ => ({
        setBeast: $.newGame(beastIndex => ({ beastIndex, dialog: beastIndex === 'custom' ? 'custombeast' : null })),
        setSquareHover: update('squareHover'),
        flipBeast: $.newGame(index => set(['customBeast', index], not)),

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
            x2: 100 * state.pointer.left / state.pointer.width,
            y2: 100 * state.pointer.top / state.pointer.height,
            color: state.selectedColor
        }
    })
});