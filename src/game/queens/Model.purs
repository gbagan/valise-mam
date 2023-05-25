module Game.Queens.Model where

import MamPrelude
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Array.NonEmpty as N
import Data.Either (note)
import Data.FoldableWithIndex (foldrWithIndex)
import Game.Core
  ( GModel
  , class MsgWithCore
  , class Game
  , class ScoreGame
  , CoreMsg
  , Objective(..)
  , Dialog(..)
  , SizeLimit(..)
  , ShowWinPolicy(..)
  , coreUpdate
  , playA
  , genModel
  , newGame
  , updateScore'
  , saveToJson'
  , loadFromJson'
  , _ext
  , _dialog
  , _position
  , _nbRows
  , _nbColumns
  )
import Lib.Update (UpdateMam)
import Lib.Helpers (count, dCoords, map2)

piecesList ∷ Array Piece
piecesList = [ Rook, Bishop, King, Knight, Queen ]

data Piece = Rook | Bishop | King | Knight | Queen | Custom | Empty

derive instance Eq Piece
instance Show Piece where
  show Queen = "queen"
  show King = "king"
  show Rook = "rook"
  show Bishop = "bishop"
  show Knight = "knight"
  show Custom = "custom"
  show Empty = "empty"

pieceFromString ∷ String → Maybe Piece
pieceFromString = case _ of
  "queen" → Just Queen
  "king" → Just King
  "rook" → Just Rook
  "bishop" → Just Bishop
  "knight" → Just Knight
  "custom" → Just Custom
  "empty" → Just Empty
  _ → Nothing

instance EncodeJson Piece where
  encodeJson = encodeJson <<< show

instance DecodeJson Piece where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "Piece") (pieceFromString string)

type Position = Array Piece
type Ext' =
  { selectedPiece ∷ Piece -- la pièce actuellement choisie par l'utilisateur
  , selectedSquare ∷ Maybe Int -- la case sur laquelle pointe le pointeur de l'utilisateur
  , allowedPieces ∷ NonEmptyArray Piece -- la liste des pièces que l'utilisateur a le droit d'utiliser
  , multiPieces ∷ Boolean -- l'utilisateur peut-il utiliser plusieurs pièces différentes ou une seule
  , customLocalMoves ∷ Array Boolean -- la liste des mouvements locaux autorisées pour la pièce personnalisée
  , customDirections ∷ Array Boolean -- la liste des directions autorisées pour la pièce personnalisée
  }

newtype Ext = Ext Ext'
type Model = GModel Position Ext

-- état initial
imodel ∷ Model
imodel = genModel []
  _
    { nbRows = 8
    , nbColumns = 8
    }
  ( Ext
      { selectedPiece: Queen
      , selectedSquare: Nothing
      , allowedPieces: N.singleton Rook
      , multiPieces: false
      , customLocalMoves: replicate 25 false
      , customDirections: replicate 9 false
      }
  )

-- lenses
_ext' ∷ Lens' Model Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext

_selectedPiece ∷ Lens' Model Piece
_selectedPiece = _ext' ∘ prop (Proxy ∷ _ "selectedPiece")

_selectedSquare ∷ Lens' Model (Maybe Int)
_selectedSquare = _ext' ∘ prop (Proxy ∷ _ "selectedSquare")

_allowedPieces ∷ Lens' Model (NonEmptyArray Piece)
_allowedPieces = _ext' ∘ prop (Proxy ∷ _ "allowedPieces")

_multiPieces ∷ Lens' Model Boolean
_multiPieces = _ext' ∘ prop (Proxy ∷ _ "multiPieces")

_customLocalMoves ∷ Lens' Model (Array Boolean)
_customLocalMoves = _ext' ∘ prop (Proxy ∷ _ "customLocalMoves")

_customDirections ∷ Lens' Model (Array Boolean)
_customDirections = _ext' ∘ prop (Proxy ∷ _ "customDirections")

-- | teste si une pièce peut se déplacer d'un vecteur (x, y)
legalMoves ∷ Piece → Int → Int → Boolean
legalMoves Queen x y = (x * x - y * y) * x * y == 0
legalMoves King x y = x * x + y * y <= 2
legalMoves Rook x y = x * y == 0
legalMoves Bishop x y = x * x - y * y == 0
legalMoves Knight x y = x * x + y * y == 5
legalMoves _ _ _ = false

-- contrairement à Data.Ord.signum, sign 0 = 0
sign ∷ Int → Int
sign 0 = 0
sign x
  | x > 0 = 1
  | otherwise = -1

-- | teste si la pièce de type "piece" à la position index1 peut attaquer la pièce à la position index2
-- | suppose que la pièce est différent de Empty
canCapture ∷ Model → Piece → Int → Int → Boolean
canCapture model piece index1 index2 =
  let
    { row, col } = dCoords (model ^. _nbColumns) index2 index1
  in
    if piece ≠ Custom then
      index1 ≠ index2 && legalMoves piece row col
    else
      (row * row - col * col) * row * col == 0 && (model ^. _customDirections) !! (3 * sign row + sign col + 4) == Just true
        || row
        * row
        + col
        * col
        <= 8
        && (model ^. _customLocalMoves)
        !! (5 * row + col + 12)
        == Just true

-- | renvoie l'ensemble des positions pouvant être attaquées par une pièce à la position index sous forme de tableau de booléens
attackedBy ∷ Model → Piece → Int → Array Boolean
attackedBy model piece index =
  repeat (model ^. _nbRows * model ^. _nbColumns) (canCapture model piece index)

-- | renvoie l'ensemble des cases pouvant être attaquées par une pièce sur le plateau
capturableSquares ∷ Model → Array Boolean
capturableSquares model = model ^. _position # foldrWithIndex
  (\index piece → if piece == Empty then identity else zipWith (||) (attackedBy model piece index))
  (replicate (model ^. _nbRows * model ^. _nbColumns) false)

-- | renvoie l'ensemble des cases attaquées par l'endroit du pointeur de la souris
attackedBySelected ∷ Model → Array Boolean
attackedBySelected model = case model ^. _selectedSquare of
  Nothing → replicate (model ^. _nbRows * model ^. _nbColumns) false
  Just index → attackedBy model (model ^. _selectedPiece) index

-- | ajoute ou enlève une pièce à la liste des pièces autorisées.
-- | Le paramètre booléen correspond à la présence du mode multipiece
toggleAllowedPiece ∷ Piece → Boolean → NonEmptyArray Piece → NonEmptyArray Piece
toggleAllowedPiece piece false _ = N.singleton piece
toggleAllowedPiece piece true pieces = N.fromArray pieces2 ?: pieces
  where
  pieces2 = piecesList # filter \p2 → (p2 == piece) ≠ elem p2 pieces

isValidPosition ∷ Model → Boolean
isValidPosition model = and $ map2 (capturableSquares model) (model ^. _position)
  \_ captured piece → not captured || piece == Empty

instance Game Position Ext Int where
  name _ = "queens"

  play model index =
    let
      selectedPiece = model ^. _selectedPiece
    in
      model ^. _position # modifyAt index \t → if t == selectedPiece then Empty else selectedPiece

  initialPosition model = pure $ replicate (model ^. _nbRows * model ^. _nbColumns) Empty

  isLevelFinished _ = false

  onNewGame model = pure $ model # set _selectedPiece (N.head $ model ^. _allowedPieces)
  sizeLimit _ = SizeLimit 3 3 9 9
  updateScore model = updateScore' { onlyWhenFinished: false, showWin: NeverShowWin } model

  -- methodes par défaut
  computerMove _ = pure Nothing
  onPositionChange = identity
  saveToJson = saveToJson'
  loadFromJson = loadFromJson'

instance ScoreGame (Array Piece) Ext Int where
  objective _ = Maximize
  scoreFn model
    | isValidPosition model = count (_ ≠ Empty) $ model ^. _position
    | otherwise = 0
  scoreHash model = joinWith "-" [ show (model ^. _nbRows), show (model ^. _nbColumns), show (N.head $ model ^. _allowedPieces) ]
  isCustomGame model = model ^. _multiPieces || N.head (model ^. _allowedPieces) == Custom

data Msg
  = Core CoreMsg
  | Play Int
  | SelectPiece Piece
  | SelectSquare (Maybe Int)
  | SelectAllowedPiece Piece
  | ToggleMultiPieces
  | FlipDirection Int
  | FlipLocalMove Int
  | Customize

instance MsgWithCore Msg where
  core = Core

update ∷ Msg → UpdateMam Model Msg Unit
update (Core msg) = coreUpdate msg
update (Play i) = playA i
update (SelectPiece piece) = _selectedPiece .= piece
update (SelectSquare a) = _selectedSquare .= a
update (SelectAllowedPiece piece) = newGame $ \model → model # over _allowedPieces (toggleAllowedPiece piece (model ^. _multiPieces))
update ToggleMultiPieces = _multiPieces %= not
update (FlipDirection direction)
  | direction /= 4 = newGame $ over (_customDirections ∘ ix direction) not
  | otherwise = pure unit
update (FlipLocalMove position)
  | position /= 12 = newGame $ over (_customLocalMoves ∘ ix position) not
  | otherwise = pure unit
update Customize = newGame
  $ set _allowedPieces (N.singleton Custom)
  ∘ set _dialog CustomDialog
  ∘ set _multiPieces false