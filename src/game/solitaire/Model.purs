module Game.Solitaire.Model where

import MamPrelude

import Data.FoldableWithIndex (allWithIndex)
import Game.Core
  ( class Game
  , class MsgWithCore
  , class MsgWithDnd
  , class ScoreGame
  , CoreMsg(ToggleHelp)
  , DndMsg
  , GModel
  , Objective(..)
  , ShowWinPolicy(..)
  , SizeLimit(..)
  , _customSize
  , _ext
  , _nbColumns
  , _nbRows
  , _position
  , canPlay
  , coreUpdate
  , dndUpdate
  , genModel
  , newGame
  , saveToJson'
  , updateScore'
  , loadFromJson'
  )
import Lib.Update (UpdateMam)
import Control.Monad.Gen (chooseBool)
import Lib.Util (count, repeat2, dCoords, chooseInt')

type Position = Array Boolean
type Move = { from ∷ Int, to ∷ Int }

data Board = FrenchBoard | EnglishBoard | CircleBoard | Grid3Board | RandomBoard

derive instance Eq Board
instance Show Board where
  show FrenchBoard = "french"
  show EnglishBoard = "english"
  show CircleBoard = "circle"
  show Grid3Board = "grid3"
  show RandomBoard = "random"

type Ext' =
  { board ∷ Board
  , holes ∷ Array Boolean
  , dragged ∷ Maybe Int
  , help ∷ Int -- 0 → pas d'aide, 1 → première tricoloration, 2 → deuxème tricoloration
  }

newtype ExtModel = Ext Ext'
type Model = GModel (Array Boolean) ExtModel

imodel ∷ Model
imodel = genModel [] _ { nbRows = 5, nbColumns = 1 } (Ext { board: CircleBoard, holes: [], dragged: Nothing, help: 0 })

_ext' ∷ Lens' Model Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext

_board ∷ Lens' Model Board
_board = _ext' ∘ prop (Proxy ∷ _ "board")

_holes ∷ Lens' Model (Array Boolean)
_holes = _ext' ∘ prop (Proxy ∷ _ "holes")

_dragged ∷ Lens' Model (Maybe Int)
_dragged = _ext' ∘ prop (Proxy ∷ _ "dragged")

_help ∷ Lens' Model Int
_help = _ext' ∘ prop (Proxy ∷ _ "help")

-- | retourne la position du trou situé entre les deux positions d'un coup si celui est valide
betweenMove ∷ Model → Move → Maybe Int
betweenMove model { from, to } =
  let
    { row, col } = dCoords (model ^. _nbColumns) from to
  in
    if row * row + col * col == 4 then Just $ (from + to) / 2 else Nothing

-- | même chose que betweenMove mais dans un plateau circulaire    
-- | ne traite pas le cas du plateau de taille 4
betweenInCircle ∷ Int → Int → Int → Maybe Int
betweenInCircle from to size =
  if from - to == 2 || to - from == 2 then
    Just $ (from + to) / 2
  else if (to - from) `mod` size == 2 then
    Just $ (from + 1) `mod` size
  else if (from - to) `mod` size == 2 then
    Just $ (to + 1) `mod` size
  else
    Nothing

-- | même chose que betweenMove dans un plateau normal ou circuaire.
-- | Traite le cas particulier du plateau circulaire de taille 4
betweenMove2 ∷ Model → Move → Maybe Int
betweenMove2 model move@{ from, to } =
  let
    rows = model ^. _nbRows
  in
    if model ^. _board == CircleBoard then do
      x ← betweenInCircle from to rows
      pure $ if rows == 4 && maybe false not (model ^. _position !! x) then (x + 2) `mod` 4 else x
    else
      betweenMove model move

-- | fonction auxilaire pour onNewGame
generateBoard
  ∷ Int
  → Int
  → Int
  → (Int → Int → Boolean)
  → Gen { holes ∷ Array Boolean, position ∷ Position, customSize ∷ Boolean }
generateBoard rows columns startingHole holeFilter = pure { holes, position, customSize: false }
  where
  holes = repeat2 rows columns holeFilter
  position = holes # set (ix startingHole) false

instance Game Position ExtModel Move where
  name _ = "solitaire"

  play model move@{ from, to } = do
    let position = model ^. _position
    between ← betweenMove2 model move
    pfrom ← position !! from
    pbetween ← position !! between
    pto ← position !! to
    hto ← model ^. _holes !! to
    guard $ pfrom && pbetween && hto && not pto
    Just $ position # updateAtIndices [ from ∧ false, between ∧ false, to ∧ true ]

  initialPosition = pure ∘ view _position

  isLevelFinished model =
    model ^. _position # allWithIndex \i _ →
      ( [ 2, -2, 2 * model ^. _nbColumns, -2 * model ^. _nbColumns, model ^. _nbRows - 2 ] # all \d →
          not canPlay model { from: i, to: i + d }
      )

  onNewGame model = do
    let columns = model ^. _nbColumns
    let rows = model ^. _nbRows
    { holes, position, customSize } <-
      case model ^. _board of
        EnglishBoard → generateBoard 7 7 24 \row col → min row (6 - row) >= 2 || min col (6 - col) >= 2
        FrenchBoard → generateBoard 7 7 24 \row col → min row (6 - row) + min col (6 - col) >= 2
        CircleBoard → do
          position <- chooseInt' rows <#> \x → repeat rows (_ ≠ x)
          pure
            { holes: replicate rows true
            , position
            , customSize: true
            }
        Grid3Board → pure
          { holes: replicate (3 * model ^. _nbColumns) true
          , position: repeat (3 * model ^. _nbColumns) (_ < 2 * columns)
          , customSize: true
          }
        RandomBoard → do
          position <- replicateA columns chooseBool <#> \bools →
            bools <> replicate columns true <> (bools <#> not)
          pure
            { holes: replicate (3 * model ^. _nbColumns) true
            , position
            , customSize: true
            }
    pure $ model
      # set _holes holes
      # set _position position
      # set _customSize customSize

  sizeLimit model = case model ^. _board of
    CircleBoard → SizeLimit 3 1 12 1
    Grid3Board → SizeLimit 3 1 3 12
    RandomBoard → SizeLimit 3 1 3 12
    _ → SizeLimit 7 7 7 7

  updateScore model = updateScore' { onlyWhenFinished: true, showWin: AlwaysShowWin } model
  saveToJson = saveToJson'
  loadFromJson = loadFromJson'

  computerMove _ = pure Nothing
  onPositionChange = identity

instance ScoreGame Position ExtModel Move where
  objective _ = Minimize
  scoreFn = count identity ∘ view _position
  scoreHash model = joinWith "-" [ show (model ^. _board), show (model ^. _nbRows), show (model ^. _nbColumns) ]
  isCustomGame model = model ^. _board == RandomBoard

data Msg = Core CoreMsg | DnD (DndMsg Int) | SetBoard Board

instance MsgWithCore Msg where
  core = Core

instance MsgWithDnd Msg Int where
  dndmsg = DnD

update ∷ Msg → UpdateMam Model Msg Unit
update (Core ToggleHelp) = _help %= \x → (x + 1) `mod` 3
update (Core msg) = coreUpdate msg
update (DnD msg) = dndUpdate _dragged msg
update (SetBoard board) = newGame \model →
  let
    st2 = model # set _board board
  in
    case board of
      CircleBoard → st2 # set _nbRows 6 # set _nbColumns 1
      Grid3Board → st2 # set _nbRows 3 # set _nbColumns 5
      RandomBoard → st2 # set _nbRows 3 # set _nbColumns 5
      _ → st2 # set _nbRows 7 # set _nbColumns 7