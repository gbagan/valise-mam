module Game.Tiling.Model where

import MamPrelude
import Lib.Util (coords)
import Game.Common (_isoCustom)
import Game.Core
  ( GModel
  , Dialog(..)
  , class Game
  , class MsgWithCore
  , CoreMsg
  , SizeLimit(..)
  , coreUpdate
  , playA
  , _ext
  , canPlay
  , genModel
  , newGame
  , _position
  , _nbColumns
  , _nbRows
  , _dialog
  , defaultUpdateScore
  )
import Lib.Update (UpdateMam)

type Coord = { row ∷ Int, col ∷ Int }
type Tile = Array Coord

data TileType = Type1 | Type2 | Type3 | CustomTile

derive instance Eq TileType
instance Show TileType where
  show Type1 = "beast1"
  show Type2 = "beast2"
  show Type3 = "beast3"
  show CustomTile = "customize"

rotate90 ∷ Tile → Tile
rotate90 = map \{ row, col } → { row: col, col: -row }

rotate ∷ Int → Tile → Tile
rotate 0 t = t
rotate i t = rotate (i - 1) (rotate90 t)

translate ∷ Coord → Tile → Tile
translate { row: drow, col: dcol } = map \{ row, col } → { row: row + drow, col: col + dcol }

-- une position représente pour chaque position dans la grille ce que contient la case
--     0: ne contient rien,
--     -1: contient un évier,
--     n > 0 contient un morceau de tuile
-- les morceaux de tuiles ayant le même numéro appartiennent à la même tuile
-- une partie est terminé si toute case contient un évier ou un morceau de tuile
-- un coup peut consister à poser une tuile ou à en retirer une

type Ext' =
  { rotation ∷ Int -- le nombre de rotations effectuées sur la tuile courrante
  , tileType ∷ TileType -- un symbole réprésentant une tuile
  , tile ∷ Tile -- une tuile représenté par un tableau de ses positions
  , nbSinks ∷ Int -- le nombre d'éviers que l'on doit poser avant de commencer la partie
  , hoverSquare ∷ Maybe Int -- la case sur laquelle passe la souris
  }

newtype ExtModel = Ext Ext'
type Model = GModel (Array Int) ExtModel

-- état initial
imodel ∷ Model
imodel = genModel [] _ { nbRows = 5, nbColumns = 5 }
  ( Ext
      { rotation: 0
      , tileType: Type1
      , tile: []
      , nbSinks: 0
      , hoverSquare: Nothing
      }
  )

-- lenses
_ext' ∷ Lens' Model Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext

_rotation ∷ Lens' Model Int
_rotation = _ext' ∘ prop (Proxy ∷ _ "rotation")

_tile ∷ Lens' Model Tile
_tile = _ext' ∘ prop (Proxy ∷ _ "tile")

_tileType ∷ Lens' Model TileType
_tileType = _ext' ∘ prop (Proxy ∷ _ "tileType")

_nbSinks ∷ Lens' Model Int
_nbSinks = _ext' ∘ prop (Proxy ∷ _ "nbSinks")

_hoverSquare ∷ Lens' Model (Maybe Int)
_hoverSquare = _ext' ∘ prop (Proxy ∷ _ "hoverSquare")

getTile ∷ Model → Tile
getTile model = case model ^. _tileType of
  Type1 → [ { row: 0, col: 0 }, { row: 0, col: 1 } ]
  Type2 → [ { row: 0, col: 0 }, { row: 0, col: 1 }, { row: 0, col: -1 } ]
  Type3 → [ { row: 0, col: 0 }, { row: 0, col: 1 }, { row: 1, col: 0 } ]
  CustomTile → model ^. _tile

-- renvoie la liste des positions où devra être posée une tuile,  -1 est une position invalide
tilePositions ∷ Model → Int → Array Int
tilePositions model index =
  model ^. _tile
    # rotate ((model ^. _rotation) `mod` 4)
    # translate (coords columns index)
    <#> \{ row, col } → if 0 <= col && col < columns then row * columns + col else -1
  where
  columns = model ^. _nbColumns

-- teste si une tuile peut être posée à partir de la liste des positions otenues par tilePositions
canPutTile ∷ Model → Array Int → Boolean
canPutTile model = all \index → model ^. _position !! index == Just 0

-- renvoie la liste des positions des éviers
sinks ∷ Model → Array Int
sinks model = model ^. _position # mapWithIndex (\i v → if v == -1 then Just i else Nothing) # catMaybes

-- teste si la tuile que l'on souhaite poser à la position du pointeur est en conflit avec les pièces déjà posées
inConflict ∷ Model → Boolean
inConflict model = case model ^. _hoverSquare of
  Nothing → false
  Just sqr → model ^. _position !! sqr ≠ Just 0 || not (canPlay model sqr)

needSinks ∷ Model → Boolean
needSinks model = length (sinks model) < model ^. _nbSinks

instance Game (Array Int) ExtModel Int where
  name _ = "tiling"

  play model index =
    -- si on peut poser la tuile à l'emplacement de la souris, on le fait
    -- on choisit un numéro non attribué m comme numéro de tuile
    if canPutTile model tilePos then
      let
        m = (foldr max 0 pos) + 1
      in
        Just $ pos # updateAtIndices (tilePos <#> (_ ∧ m))
    -- si la souris se trouve sur l'emplacement d'une tuile, on retire la tuile
    else if model ^. _position !! index > Just 0 then
      Just $ pos <#> \x → if Just x == pos !! index then 0 else x
    -- sinon, c'est un coup invalide
    else
      Nothing
    where
    pos = model ^. _position
    tilePos = tilePositions model index

  isLevelFinished = all (_ ≠ 0) ∘ view _position

  initialPosition model = pure $ replicate (model ^. _nbRows * model ^. _nbColumns) 0

  sizeLimit _ = SizeLimit 3 3 10 10

  onNewGame model = pure $ model
    # set _tile (getTile model)
    # set _rotation 0

  -- méthodes par défaut
  computerMove _ = pure Nothing
  updateScore s = defaultUpdateScore s
  onPositionChange = identity
  saveToJson _ = Nothing
  loadFromJson model _ = model

data Msg
  = Core CoreMsg
  | Play Int
  | PutSink Int
  | SetNbSinks Int
  | SetTile TileType
  | Rotate
  | SetHoverSquare (Maybe Int)
  | FlipTile Int

instance MsgWithCore Msg where
  core = Core

update ∷ Msg → UpdateMam Model Msg Unit
update (Core msg) = coreUpdate msg
update (Play m) = playA m
update (PutSink i) = (_position ∘ ix i) .= (-1)
update (SetNbSinks n) = newGame (set _nbSinks n)
update (SetTile t) = newGame $ set _tileType t >>> if t == CustomTile then set _dialog CustomDialog else identity
update Rotate = _rotation += 1
update (SetHoverSquare a) = _hoverSquare .= a
update (FlipTile index) = newGame $ over (_tile ∘ _isoCustom ∘ ix index) not

onKeyDown ∷ String → Maybe Msg
onKeyDown " " = Just Rotate
onKeyDown _ = Nothing
