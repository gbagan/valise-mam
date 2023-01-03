module Game.Solitaire.Model where

import MamPrelude

import Data.FoldableWithIndex (allWithIndex)
import Game.Core (class Game, class MsgWithCore, class MsgWithDnd, class ScoreGame, 
                CoreMsg(ToggleHelp), DndMsg, GState, Objective(..), ShowWinPolicy(..), SizeLimit(..),
                _customSize, _ext, _nbColumns, _nbRows, _position, canPlay, coreUpdate, dndUpdate, genState, newGame, 
                saveToJson', updateScore', loadFromJson')
import Lib.Update (UpdateMam)
import Control.Monad.Gen (chooseBool)
import Lib.Util (repeat2, dCoords, chooseInt')

type Position = Array Boolean
type Move = {from ∷ Int, to ∷ Int}

data Board = FrenchBoard | EnglishBoard | CircleBoard | Grid3Board | RandomBoard
derive instance Eq Board
instance Show Board where
    show FrenchBoard = "french"
    show EnglishBoard = "english"
    show CircleBoard = "circle"
    show Grid3Board = "grid3"
    show RandomBoard = "random"

type Ext' = {
    board ∷ Board,
    holes ∷ Array Boolean,
    dragged ∷ Maybe Int,
    help ∷ Int -- 0 → pas d'aide, 1 → première tricoloration, 2 → deuxème tricoloration
}

newtype ExtState = Ext Ext'
type State = GState (Array Boolean) ExtState

istate ∷ State
istate = genState [] _{nbRows = 5, nbColumns = 1} (Ext { board: CircleBoard, holes: [], dragged: Nothing, help: 0 })

_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_board ∷ Lens' State Board
_board = _ext' ∘ prop (Proxy ∷ _ "board")
_holes ∷ Lens' State (Array Boolean)
_holes = _ext' ∘ prop (Proxy ∷ _ "holes")
_dragged ∷ Lens' State (Maybe Int)
_dragged = _ext' ∘ prop (Proxy ∷ _ "dragged")
_help ∷ Lens' State Int
_help = _ext' ∘ prop (Proxy ∷ _ "help")

-- | retourne la position du trou situé entre les deux positions d'un coup si celui est valide
betweenMove ∷ State → Move → Maybe Int
betweenMove state { from, to } = 
    let {row, col} = dCoords (state^._nbColumns) from to in
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
betweenMove2 ∷ State → Move → Maybe Int
betweenMove2 state move@{from, to} =
    let rows = state ^._nbRows in
    if state^._board == CircleBoard then do
        x ← betweenInCircle from to rows
        pure $ if rows == 4 && maybe false not (state^._position !! x) then (x + 2) `mod` 4 else x
    else
        betweenMove state move

-- | fonction auxilaire pour onNewGame
generateBoard ∷ ∀m. MonadGen m ⇒ Int → Int → Int → (Int → Int → Boolean) →
    m {holes ∷ Array Boolean, position ∷  Position, customSize ∷ Boolean}
generateBoard rows columns startingHole holeFilter = pure {holes, position, customSize: false} where
    holes = repeat2 rows columns holeFilter
    position = holes # set (ix startingHole) false

instance Game Position ExtState Move where
    name _ = "solitaire"

    play state move@{from, to} = do
        let position = state^._position
        between ← betweenMove2 state move
        pfrom ← position !! from
        pbetween ← position !! between
        pto ← position !! to
        hto ← state^._holes !! to
        guard $ pfrom && pbetween && hto && not pto
        Just $ position # updateAtIndices [from ∧ false, between ∧ false, to ∧ true]

    initialPosition = pure ∘ view _position

    isLevelFinished state =
        state^._position # allWithIndex \i _ →
            ([2, -2, 2 * state^._nbColumns, -2 * state^._nbColumns, state^._nbRows - 2] # all \d →
                not canPlay state { from: i, to: i + d }
            )

    onNewGame state = do
        let columns = state^._nbColumns
        let rows = state^._nbRows
        {holes, position, customSize} <-
            case state^._board of
                EnglishBoard → generateBoard 7 7 24 \row col → min row (6 - row) >= 2 || min col (6 - col) >= 2
                FrenchBoard → generateBoard 7 7 24 \row col → min row (6 - row) + min col (6 - col) >= 2
                CircleBoard → do
                    position <- chooseInt' rows <#> \x → repeat rows (_ ≠ x)
                    pure {  holes: replicate rows true
                         ,  position
                         ,  customSize: true
                         }
                Grid3Board → pure
                    {   holes: replicate (3 * state^._nbColumns) true
                    ,   position: repeat (3 * state^._nbColumns) (_ < 2 * columns)
                    ,   customSize: true
                    }
                RandomBoard → do
                    position <- replicateA columns chooseBool <#> \bools →
                                bools <> replicate columns true <> (bools <#> not)
                    pure {  holes: replicate (3 * state^._nbColumns) true
                         ,  position
                         ,  customSize: true
                         }
        pure $ state 
                # set _holes holes 
                # set _position position
                # set _customSize customSize

    sizeLimit state = case state^._board of
        CircleBoard → SizeLimit 3 1 12 1
        Grid3Board → SizeLimit 3 1 3 12
        RandomBoard → SizeLimit 3 1 3 12
        _ → SizeLimit 7 7 7 7

    updateScore st = updateScore' {onlyWhenFinished: true, showWin: AlwaysShowWin} st
    saveToJson = saveToJson'
    loadFromJson = loadFromJson'

    computerMove _ = pure Nothing
    onPositionChange = identity

instance ScoreGame Position ExtState Move where
    objective _ = Minimize
    scoreFn = length ∘ filter identity ∘ view _position
    scoreHash state = joinWith "-" [show (state^._board), show (state^._nbRows), show (state^._nbColumns)]
    isCustomGame state = state^._board == RandomBoard

data Msg = Core CoreMsg | DnD (DndMsg Int) | SetBoard Board
instance MsgWithCore Msg where core = Core
instance MsgWithDnd Msg Int where dndmsg = DnD  
    
update ∷ Msg → UpdateMam State Unit
update (Core ToggleHelp) = _help %= \x → (x + 1) `mod` 3
update (Core msg) = coreUpdate msg
update (DnD msg) = dndUpdate _dragged msg
update (SetBoard board) = newGame \state →
    let st2 = state # set _board board in 
    case board of
        CircleBoard → st2 # set _nbRows 6 # set _nbColumns 1
        Grid3Board → st2 # set _nbRows 3 # set _nbColumns 5
        RandomBoard → st2 # set _nbRows 3 # set _nbColumns 5
        _ → st2 # set _nbRows 7 # set _nbColumns 7