module Game.Tiling.Model where

import Prelude
import Data.Lens (Lens', lens, view, (.~), (^.))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), maybe)
import Data.Array ((!!), all, catMaybes, elem, foldl, mapWithIndex, replicate)
import Lib.Util (coords)
import Game.Core (State(..), class Game, SizeLimit(..), genState, _position, _nbColumns, _nbRows)
import Pha.Class (Action)
import Pha.Action (action)

type Coord = {row :: Int, col :: Int}
type Tile = Array Coord

tiles :: Array Tile
tiles = [
    [{row: 0, col: 0}, {row: 0, col: 1}],
    [{row: 0, col: 0}, {row: 0, col: 1}, {row: 0, col: -1}],
    [{row: 0, col: 0}, {row: 0, col: 1}, {row: 1, col: 1}]
]

--const fromCustomTile = customTile =>
--    repeat2(5, 5, (row, col, i) => customTile[i] ? [row - 2, col - 2] : null)
--    |> filter(x => x !== null); 

rotate90 :: Tile -> Tile
rotate90 = map \{row, col} -> {row: col, col: -row}

rotate :: Int -> Tile -> Tile
rotate 0 t = t
rotate i t = rotate (i-1) (rotate90 t)

translate :: Coord -> Tile -> Tile
translate {row: drow, col: dcol} = map \{row, col} -> {row: row + drow, col: col + dcol}


type Ext' = {
    rotation :: Int,
    tile :: Tile
}

newtype ExtState = Ext Ext'
type TilingState = State (Array Int) ExtState

tilingState :: TilingState
tilingState = genState [] (_{nbRows = 5, nbColumns = 1}) (Ext { rotation: 0, tile: [] })

_ext :: Lens' TilingState Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_rotation :: Lens' TilingState Int
_rotation = _ext <<< lens (_.rotation) (_{rotation = _})
_tile :: Lens' TilingState Tile
_tile = _ext <<< lens (_.tile) (_{tile = _})

-- renvoie la liste des positions où devra être posée une tuile,  -1 est une position invalide
placeTile :: TilingState -> Int -> Array Int
placeTile state index = 
    state^._tile
    # rotate ((state^._rotation) `mod` 4)
    # translate (coords columns index)
    <#> \{row, col} -> if 0 <= col && col < columns then row * columns + col else -1
    where columns = state^._nbColumns

-- teste si une tuile peut être posée à partir de la liste des positions otenues par placeTile
canPutTile :: TilingState -> Array Int -> Boolean
canPutTile state = all \index -> maybe false (eq 0) (state^._position !! index)

-- renvoie la liste des positions des éviers
sinks :: TilingState -> Array Int
sinks state = state^._position # mapWithIndex (\i v -> if v == -1 then Just i else Nothing) # catMaybes

instance tilingGame :: Game (Array Int) ExtState Int where
    canPlay state index = canPutTile state (placeTile state index) || (state^._position !! index # maybe false (_ > 0))

    play state index =
        let pos = state^._position 
            tilePos = placeTile state index
        in
        if canPutTile state tilePos then
            let m = (foldl max 0 pos) + 1 in
            pos # mapWithIndex \i x -> if elem i tilePos then m else x
        else
            pos <#> \x -> if Just x == pos !! index then 0 else x

    isLevelFinished = view _position >>> all (notEq 0)

    initialPosition state = pure $ replicate (state^._nbRows * state^._nbColumns) 0

    sizeLimit _ = SizeLimit 3 3 10 10

    onNewGame = pure
    computerMove _ = Nothing
  

putSinkA :: Int -> Action TilingState
putSinkA i = action $ (_position <<< ix i) .~ (-1)

{-            
const configHash = state => `${state.rows}-${state.columns}-${state.tileIndex}-${state.nbSinks}`

const rotateA = set('rotation', inc);
const toggleColoringVisible = set('coloringVisible', not);

const canPlay = 

const updateSuccess = state => state |> set(
    ['succeeded', configHash(state)],
    v => (v || repeat(state.rows * state.columns, false)) |> set(sinks(state)[0], true)
);
    
        newGame: state => ({
            tile: state.tileIndex === 'custom' ? fromCustomTile(state.customTile) : tiles[state.tileIndex],
            rotation: 0,
            coloringVisible: false,
            hoverSquare: null,
        }),
    },

    state: {
        rows: 5,
        columns : 6,
        tileIndex: 0,
        nbSinks: 0,
        succeeded: {},
        customTile: repeat(25, false) |> set(12, true)
    },
    
    actions: $ => ({
        setTile: $.newGame(tileIndex => ({ tileIndex, dialog: tileIndex === 'custom' ? 'customtile' : null })),
        setSinks: $.newGame('nbSinks'),
        toggleColoringVisible,
        setHoverSquare: update('hoverSquare'),
        flipTile: $.newGame(index => set(['customTile', index], not)),

        ////// todo concat if
        whenLevelFinished: state => state |> updateSuccess |> $.newGame(null, {noDialog: true}),

        clickOnCell: when(s => sinks(s).length < s.nbSinks, putSink, $.play),

        keyDown: keyEvents({
            ' ': rotateA,
            b: toggleColoringVisible
        })
    }),

    computed: state => ({
        inConflict: state.position[state.hoverSquare] !== 0 || !canPlay(state, state.hoverSquare),
        successForThisConf: state.succeeded[configHash(state)] || repeat(state.columns * state.rows, false),
        sinks: sinks(state)
    })
});