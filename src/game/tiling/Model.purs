module Game.Tiling.Model where

import MyPrelude
import Lib.Util (coords)
import Game.Common (_isoCustom)
import Game.Core (GState, Dialog(..), class Game, class MsgWithCore, CoreMsg, SizeLimit(..),
            coreUpdate, playA,     
            _ext, canPlay, genState, newGame, _position, _nbColumns, _nbRows, _dialog)
import Lib.Update (Update, modify)

type Coord = {row ∷ Int, col ∷ Int}
type Tile = Array Coord

data TileType = Type1 | Type2 | Type3 | CustomTile
derive instance eqTileType ∷ Eq TileType
instance showTileType ∷ Show TileType where
    show Type1 = "beast1"
    show Type2 = "beast2"
    show Type3 = "beast3"
    show CustomTile = "customize"

rotate90 ∷ Tile → Tile
rotate90 = map \{row, col} → {row: col, col: -row}

rotate ∷ Int → Tile → Tile
rotate 0 t = t
rotate i t = rotate (i-1) (rotate90 t)

translate ∷ Coord → Tile → Tile
translate {row: drow, col: dcol} = map \{row, col} → {row: row + drow, col: col + dcol}

-- une position représente pour chaque position dans la grille ce que contient la case
--     0: ne contient rien,
--     -1: contient un évier,
--     n > 0 contient un morceau de tuile
-- les morceaux de tuiles ayant le même numéro appartiennent à la même tuile
-- une partie est terminé si toute case contient un évier ou un morceau de tuile
-- un coup peut consister à poser une tuile ou à en retirer une

type Ext' = 
    {   rotation ∷ Int          -- le nombre de rotations effectuées sur la tuile courrante
    ,   tileType ∷ TileType     -- un symbole réprésentant une tuile
    ,   tile ∷ Tile             -- une tuile représenté par un tableau de ses positions
    ,   nbSinks ∷ Int           -- le nombre d'éviers que l'on doit poser avant de commencer la partie
    ,   hoverSquare ∷ Maybe Int -- la case sur laquelle passe la souris
    }

newtype ExtState = Ext Ext'
type State = GState (Array Int) ExtState

-- état initial
istate ∷ State
istate = genState [] _{nbRows = 5, nbColumns = 5}
    (Ext 
        {   rotation: 0
        ,   tileType: Type1
        ,   tile: []
        ,   nbSinks: 0 
        ,   hoverSquare: Nothing
        }
    )

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_rotation ∷ Lens' State Int
_rotation = _ext' ∘ prop (SProxy ∷ _ "rotation")
_tile ∷ Lens' State Tile
_tile = _ext' ∘ prop (SProxy ∷ _ "tile")
_tileType ∷ Lens' State TileType
_tileType = _ext' ∘ prop (SProxy ∷ _ "tileType")
_nbSinks ∷ Lens' State Int
_nbSinks = _ext' ∘ prop (SProxy ∷ _ "nbSinks")
_hoverSquare ∷ Lens' State (Maybe Int)
_hoverSquare = _ext' ∘ prop (SProxy ∷ _ "hoverSquare")

getTile ∷ State → Tile
getTile state = case state^._tileType of
    Type1 → [{row: 0, col: 0}, {row: 0, col: 1}]
    Type2 → [{row: 0, col: 0}, {row: 0, col: 1}, {row: 0, col: -1}]
    Type3 → [{row: 0, col: 0}, {row: 0, col: 1}, {row: 1, col: 0}]
    CustomTile → state^._tile

-- renvoie la liste des positions où devra être posée une tuile,  -1 est une position invalide
tilePositions ∷ State → Int → Array Int
tilePositions state index = 
    state^._tile
    # rotate ((state^._rotation) `mod` 4)
    # translate (coords columns index)
    <#> \{row, col} → if 0 <= col && col < columns then row * columns + col else -1
    where columns = state^._nbColumns

-- teste si une tuile peut être posée à partir de la liste des positions otenues par tilePositions
canPutTile ∷ State → Array Int → Boolean
canPutTile state = all \index → state^._position !! index == Just 0

-- renvoie la liste des positions des éviers
sinks ∷ State → Array Int
sinks state = state^._position # mapWithIndex (\i v → if v == -1 then Just i else Nothing) # catMaybes

-- teste si la tuile que l'on souhaite poser à la position du pointeur est en conflit avec les pièces déjà posées
inConflict ∷ State → Boolean
inConflict state = case state^._hoverSquare of
    Nothing → false
    Just sqr → state^._position !! sqr ≠ Just 0 || not (canPlay state sqr)

needSinks ∷ State → Boolean
needSinks state = length (sinks state) < state^._nbSinks

instance tilingGame ∷ Game (Array Int) ExtState Int where
    name _ = "tiling"

    play state index =
        -- si on peut poser la tuile à l'emplacement de la souris, on le fait
        -- on choisit un numéro non attribué m comme numéro de tuile
        if canPutTile state tilePos then
            let m = (foldr max 0 pos) + 1 in
            Just $ pos # updateAtIndices (tilePos <#> (_ ∧ m))
        -- si la souris se trouve sur l'emplacement d'une tuile, on retire la tuile
        else if state^._position !! index > Just 0 then
            Just $ pos <#> \x → if Just x == pos !! index then 0 else x
        -- sinon, c'est un coup invalide
        else
            Nothing
        where
            pos = state^._position 
            tilePos = tilePositions state index

    isLevelFinished = all (_ ≠ 0) ∘ view _position

    initialPosition state = pure $ replicate (state^._nbRows * state^._nbColumns) 0

    sizeLimit = const (SizeLimit 3 3 10 10)

    onNewGame state = pure $ state 
                            # set _tile (getTile state) 
                            # set _rotation 0
    computerMove _ = pure Nothing
    updateScore st = st ∧ true
    onPositionChange = identity
    saveToJson _ = Nothing
    loadFromJson st _ = st
  
data Msg = Core CoreMsg | Play Int | PutSink Int | SetNbSinks Int | SetTile TileType | Rotate
           | SetHoverSquare (Maybe Int) | FlipTile Int
instance withcore ∷ MsgWithCore Msg where core = Core
      
update ∷ Msg → Update State
update (Core msg) = coreUpdate msg  
update (Play m) = playA m
update (PutSink i) = modify $ set (_position ∘ ix i) (-1)
update (SetNbSinks n) = newGame (set _nbSinks n)
update (SetTile t) = newGame $ set _tileType t >>> if t == CustomTile then set _dialog CustomDialog else identity
update Rotate = modify $ over _rotation (_ + 1)
update (SetHoverSquare a) = modify $ set _hoverSquare a
update (FlipTile index) = newGame $ over (_tile ∘ _isoCustom ∘ ix index) not

onKeyDown ∷ String → Maybe Msg
onKeyDown " " = Just Rotate
onKeyDown _ = Nothing

{-            
const configHash = state ⇒ `${state.rows}-${state.columns}-${state.tileIndex}-${state.nbSinks}`

const updateSuccess = state ⇒ state |> set(
    ['succeeded', configHash(state)],
    v ⇒ (v || repeat(state.rows * state.columns, false)) |> set(sinks(state)[0], true)
);
        ////// todo concat if
        whenLevelFinished: state ⇒ state |> updateSuccess |> $.newGame(null, {noDialog: true}),

        successForThisConf: state.succeeded[configHash(state)] || repeat(state.columns * state.rows, false),
