module Game.Tiling.Model where

import MyPrelude
import Lib.Util (coords)
import Game.Common (_isoCustom)
import Game.Core (GState, Dialog(..), class Game, class MsgWithCore, CoreMsg, SizeLimit(..),
            coreUpdate, playA,     
            _ext, canPlay, genState, newGame, _position, _nbColumns, _nbRows, _dialog)
import Pha.Action (Action, setState)
import Game.Effs (EFFS)

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
    hoverSquare :: Maybe Int
}

newtype ExtState = Ext Ext'
type State = GState (Array Int) ExtState

-- état initial
istate :: State
istate = genState [] _{nbRows = 5, nbColumns = 5}
    (Ext {
        rotation: 0,
        tileType: Type1, 
        tile: [],
        nbSinks: 0, 
        hoverSquare: Nothing
    })

-- lenses
_ext' :: Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) -> a) Ext
_rotation :: Lens' State Int
_rotation = _ext' ∘ lens _.rotation _{rotation = _}
_tile :: Lens' State Tile
_tile = _ext' ∘ lens _.tile _{tile = _}
_tileType :: Lens' State TileType
_tileType = _ext' ∘ lens _.tileType _{tileType = _}
_nbSinks :: Lens' State Int
_nbSinks = _ext' ∘ lens _.nbSinks _{nbSinks = _}
_hoverSquare :: Lens' State (Maybe Int)
_hoverSquare = _ext' ∘ lens _.hoverSquare _{hoverSquare = _}

getTile :: State -> Tile
getTile state = case state^._tileType of
    Type1 -> [{row: 0, col: 0}, {row: 0, col: 1}]
    Type2 -> [{row: 0, col: 0}, {row: 0, col: 1}, {row: 0, col: -1}]
    Type3 -> [{row: 0, col: 0}, {row: 0, col: 1}, {row: 1, col: 0}]
    CustomTile -> state^._tile

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
canPutTile state = all \index -> state^._position !! index == Just 0

-- renvoie la liste des positions des éviers
sinks :: State -> Array Int
sinks state = state^._position # mapWithIndex (\i v -> if v == -1 then Just i else Nothing) # catMaybes

-- teste si la tuile que l'on souhaite poser à la position du pointeur est en conflit avec les pièces déjà posées
inConflict :: State -> Boolean
inConflict state = case state^._hoverSquare of
    Nothing -> false
    Just sqr -> state^._position !! sqr /= Just 0 || not (canPlay state sqr)

needSinks :: State -> Boolean
needSinks state = length (sinks state) < state^._nbSinks

instance tilingGame :: Game (Array Int) ExtState Int where
    play state index =
        let pos = state^._position 
            tilePos = placeTile state index
        in
        if canPutTile state tilePos then
            let m = (foldr max 0 pos) + 1 in
            Just $ pos # updateAtIndices (tilePos <#> (_ ∧ m))
        else if state^._position !! index > Just 0 then
            Just $ pos <#> \x -> if Just x == pos !! index then 0 else x
        else
            Nothing

    isLevelFinished = all (_ /= 0) ∘ view _position

    initialPosition state = pure $ replicate (state^._nbRows * state^._nbColumns) 0

    sizeLimit = const (SizeLimit 3 3 10 10)

    onNewGame state = pure $ state # _tile .~ getTile state # _rotation .~ 0
    computerMove _ = pure Nothing
    updateScore st = st ∧ true 
  
data Msg = Core CoreMsg | Play Int | PutSink Int | SetNbSinks Int | SetTile TileType | Rotate
           | SetHoverSquare (Maybe Int) | FlipTile Int
instance withcore :: MsgWithCore Msg where core = Core
      
update :: Msg -> Action State EFFS
update (Core msg) = coreUpdate msg  
update (Play m) = playA m
update (PutSink i) = setState $ _position ∘ ix i .~ (-1)
update (SetNbSinks n) = newGame (_nbSinks .~ n)
update (SetTile t) = newGame $ (_tileType .~ t) >>> (if t == CustomTile then _dialog .~ CustomDialog else identity)
update Rotate = setState $ _rotation %~ (_ + 1)
update (SetHoverSquare a) = setState (_hoverSquare .~ a)
update (FlipTile index) = newGame $ _tile ∘ _isoCustom ∘ ix index %~ not

onKeyDown :: String -> Maybe Msg
onKeyDown " " = Just Rotate
onKeyDown _ = Nothing
--onKeyDown _ = pure unit

{-            
const configHash = state => `${state.rows}-${state.columns}-${state.tileIndex}-${state.nbSinks}`

const updateSuccess = state => state |> set(
    ['succeeded', configHash(state)],
    v => (v || repeat(state.rows * state.columns, false)) |> set(sinks(state)[0], true)
);
        ////// todo concat if
        whenLevelFinished: state => state |> updateSuccess |> $.newGame(null, {noDialog: true}),

        successForThisConf: state.succeeded[configHash(state)] || repeat(state.columns * state.rows, false),
