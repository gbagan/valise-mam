module Game.Tiling.Model where

import MyPrelude
import Data.Array (catMaybes)
import Lib.Util (coords)
import Game.Core (GState(..), Dialog(..), class Game, SizeLimit(..),
                  canPlay, genState, newGame, newGame', playA, _position, _nbColumns, _nbRows, _dialog)
import Pha.Action (Action, action, RNG, DELAY, getState)

type Coord = {row :: Int, col :: Int}
type Tile = Array Coord

data TileType = Type1 | Type2 | Type3 | CustomTile
derive instance eqTileType :: Eq TileType
instance showTileType :: Show TileType where
    show Type1 = "beast1"
    show Type2 = "beast2"
    show Type3 = "beast3"
    show CustomTile = "customize"

rotate90 :: Tile -> Tile
rotate90 = map \{row, col} -> {row: col, col: -row}

rotate :: Int -> Tile -> Tile
rotate 0 t = t
rotate i t = rotate (i-1) (rotate90 t)

translate :: Coord -> Tile -> Tile
translate {row: drow, col: dcol} = map \{row, col} -> {row: row + drow, col: col + dcol}

type Ext' = {
    rotation :: Int,
    tileType :: TileType,
    tile :: Tile,
    nbSinks :: Int,
    hoverSquare :: Maybe Int,
    customTile :: Array Boolean
}

newtype ExtState = Ext Ext'
type State = GState (Array Int) ExtState

istate :: State
istate = genState [] (_{nbRows = 5, nbColumns = 5}) (Ext { rotation: 0, tileType: Type1, tile: [], nbSinks: 0, 
                    customTile: replicate 25 false # ix 12 .~ true, hoverSquare: Nothing })

_ext :: Lens' State Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_rotation :: Lens' State Int
_rotation = _ext ∘ lens (_.rotation) (_{rotation = _})
_tile :: Lens' State Tile
_tile = _ext ∘ lens (_.tile) (_{tile = _})
_tileType :: Lens' State TileType
_tileType = _ext ∘ lens (_.tileType) (_{tileType = _})
_customTile :: Lens' State (Array Boolean)
_customTile = _ext ∘ lens (_.customTile) (_{customTile = _})
_nbSinks :: Lens' State Int
_nbSinks = _ext ∘ lens (_.nbSinks) (_{nbSinks = _})
_hoverSquare :: Lens' State (Maybe Int)
_hoverSquare = _ext ∘ lens (_.hoverSquare) (_{hoverSquare = _})


getTile :: State -> Tile
getTile state = case state^._tileType of
    Type1 -> [{row: 0, col: 0}, {row: 0, col: 1}]
    Type2 -> [{row: 0, col: 0}, {row: 0, col: 1}, {row: 0, col: -1}]
    Type3 -> [{row: 0, col: 0}, {row: 0, col: 1}, {row: 1, col: 0}]
    CustomTile -> state^._customTile # mapWithIndex (\i b -> if b then Just {row: i / 5 - 2, col: i `mod` 5 - 2} else Nothing) # catMaybes
        --    |> filter(x => x !== null); 

-- renvoie la liste des positions où devra être posée une tuile,  -1 est une position invalide
placeTile :: State -> Int -> Array Int
placeTile state index = 
    state^._tile
    # rotate ((state^._rotation) `mod` 4)
    # translate (coords columns index)
    <#> \{row, col} -> if 0 <= col && col < columns then row * columns + col else -1
    where columns = state^._nbColumns

-- teste si une tuile peut être posée à partir de la liste des positions otenues par placeTile
canPutTile :: State -> Array Int -> Boolean
canPutTile state = all \index -> maybe false (eq 0) (state^._position !! index)

-- renvoie la liste des positions des éviers
sinks :: State -> Array Int
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

    isLevelFinished = all (notEq 0) ∘ view _position

    initialPosition state = pure $ replicate (state^._nbRows * state^._nbColumns) 0

    sizeLimit = const (SizeLimit 3 3 10 10)

    onNewGame state = pure $ state # _tile .~ getTile state # _rotation .~ 0
--      hoverSquare: null,
    computerMove _ = Nothing
  

putSinkA :: ∀effs. Int -> Action State effs
putSinkA i = action $ (_position ∘ ix i) .~ (-1)

setNbSinksA :: ∀effs. Int -> Action State (rng :: RNG | effs)
setNbSinksA = newGame' (set _nbSinks)

setTileA :: ∀effs. TileType -> Action State (rng :: RNG | effs)
setTileA ttype = newGame $ (_tileType .~ ttype) ∘ (if ttype == CustomTile then _dialog .~ CustomDialog else identity)

clickOnCellA :: ∀effs. Int -> Action State (rng :: RNG, delay :: DELAY | effs)
clickOnCellA a = do
    state <- getState
    if length (sinks state) < state^._nbSinks then
        putSinkA a
    else
        playA a

rotateA :: ∀effs. Action State effs
rotateA = action $ _rotation %~ (add 1)

setHoverSquareA :: ∀effs. Maybe Int -> Action State effs
setHoverSquareA a = action $ _hoverSquare .~ a

inConflict :: State -> Boolean
inConflict state = state^._hoverSquare # maybe false \sqr -> state^._position !! sqr /= Just 0 || not canPlay state sqr

onKeyDown :: ∀effs. String -> Action State effs
onKeyDown " " = rotateA
onKeyDown _ = pure unit

flipTileA :: ∀effs. Int -> Action State (rng :: RNG | effs)
flipTileA index = newGame $ (_customTile ∘ ix index) %~ not

{-            
const configHash = state => `${state.rows}-${state.columns}-${state.tileIndex}-${state.nbSinks}`

const toggleColoringVisible = set('coloringVisible', not);

const updateSuccess = state => state |> set(
    ['succeeded', configHash(state)],
    v => (v || repeat(state.rows * state.columns, false)) |> set(sinks(state)[0], true)
);
     todo todo

    },

    state: {
        customTile: repeat(25, false) |> set(12, true)
    },
    
    actions: $ => ({
        setTile: $.newGame(tileIndex => ({ tileIndex, dialog: tileIndex === 'custom' ? 'customtile' : null })),
        

        ////// todo concat if
        whenLevelFinished: state => state |> updateSuccess |> $.newGame(null, {noDialog: true}),

        successForThisConf: state.succeeded[configHash(state)] || repeat(state.columns * state.rows, false),
