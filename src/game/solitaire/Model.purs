module Game.Solitaire.Model where
import MyPrelude
import Data.FoldableWithIndex (allWithIndex)
import Pha.Action (Action, RNG, setState)
import Run (Run)
import Pha.Random (randomInt, randomBool)
import Lib.Util (tabulate, tabulate2, dCoords)
import Game.Core (class Game, class ScoreGame, GState, SizeLimit(..), Objective(..), ShowWinStrategy(..),
                  _ext, genState, canPlay, _nbColumns, _nbRows, _customSize, _position, newGame, updateScore')

type Move = {from :: Int, to :: Int}

data Board = FrenchBoard | EnglishBoard | CircleBoard | Grid3Board | RandomBoard
derive instance boardMode :: Eq Board
instance showMode :: Show Board where
    show FrenchBoard = "french"
    show EnglishBoard = "english"
    show CircleBoard = "circle"
    show Grid3Board = "grid3"
    show RandomBoard = "random"

type Ext' = {
    board :: Board,
    holes :: Array Boolean,
    dragged :: Maybe Int,
    help :: Int -- 0 -> pas d'aide, 1 -> première tricoloration, 2 -> deuxème tricoloration
}

newtype ExtState = Ext Ext'
type State = GState (Array Boolean) ExtState

istate :: State
istate = genState [] _{nbRows = 5, nbColumns = 1} (Ext { board: CircleBoard, holes: [], dragged: Nothing, help: 0 })

_ext' :: Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) -> a) Ext
_board :: Lens' State Board
_board = _ext' ∘ lens _.board _{board = _}
_holes :: Lens' State (Array Boolean)
_holes = _ext' ∘ lens _.holes _{holes = _}
_dragged :: Lens' State (Maybe Int)
_dragged = _ext' ∘ lens _.dragged _{dragged = _}
_help :: Lens' State Int
_help = _ext' ∘ lens _.help _{help = _}

-- retourne la position du trou situé entre les deux positions d'un coup si celui est valide
betweenMove :: State -> Move -> Maybe Int
betweenMove state { from, to } = 
    let {row, col} = dCoords (state^._nbColumns) from to in
    if row * row + col * col == 4 then Just $ (from + to) / 2 else Nothing

-- même chose que betweenMove mais dans un plateau circulaire    
betweenInCircle :: Int -> Int -> Int -> Maybe Int
betweenInCircle from to size =
    if from - to == 2 || to - from == 2 then
        Just $ (from + to) / 2
    else if (to - from) `mod` size == 2 then
        Just $ (from + 1) `mod` size
    else if (from - to) `mod` size == 2 then
        Just $ (to + 1) `mod` size
    else
        Nothing

-- même chose que betweenMove dans un plateau normal ou circuaire. Traite le cas pariculier du plateau circulaire de taille 4
betweenMove2 :: State -> Move -> Maybe Int
betweenMove2 state move@{from, to} =
    let rows = state ^._nbRows in
    if state^._board == CircleBoard then do
        x <- betweenInCircle from to rows
        pure $ if rows == 4 && maybe false not (state^._position !! x) then (x + 2) `mod` 4 else x
    else
        betweenMove state move

-- fonction auxilaire pour onNewGame
generateBoard :: forall r. Int -> Int -> Int -> (Int -> Int -> Boolean) ->
    {holes :: Array Boolean, position :: Run (rng :: RNG | r) (Array Boolean), customSize :: Boolean}
generateBoard rows columns startingHole holeFilter = {holes, position, customSize: false} where
    holes = tabulate2 rows columns holeFilter
    position = pure $ holes # ix startingHole .~ false

instance solitaireGame :: Game (Array Boolean) ExtState {from :: Int, to :: Int} where
    play state move@{from, to} = do
        let position = state^._position
        between <- betweenMove2 state move
        pfrom <- position !! from
        pbetween <- position !! between
        pto <- position !! to
        hto <- state^._holes !! to
        if pfrom && pbetween && hto && not pto then
            Just $ position # updateAtIndices [from ∧ false, between ∧ false, to ∧ true]
        else
            Nothing

    initialPosition = pure ∘ view _position

    isLevelFinished state =
        state^._position # allWithIndex \i val ->
            ([2, -2, 2 * state^._nbColumns, -2 * state^._nbColumns, state^._nbRows - 2] # all \d ->
                not canPlay state { from: i, to: i + d }
            )

    onNewGame state = position <#> \p -> state # _holes .~ holes # _position .~ p # _customSize .~ customSize where
        columns = state^._nbColumns
        rows = state^._nbRows
        {holes, position, customSize} =
            case state^._board of
                EnglishBoard -> generateBoard 7 7 24 \row col -> min row (6 - row) >= 2 || min col (6 - col) >= 2
                FrenchBoard -> generateBoard 7 7 24 \row col -> min row (6 - row) + min col (6 - col) >= 2
                CircleBoard -> {
                    holes: replicate rows true,
                    position: randomInt rows <#> \x -> tabulate rows (_ /= x),
                    customSize: true

                }
                Grid3Board -> {
                    holes: replicate (3 * state^._nbColumns) true,
                    position: pure $ tabulate (3 * state^._nbColumns) (_ < 2 * columns),
                    customSize: true
                }
                RandomBoard -> {
                    holes: replicate (3 * state^._nbColumns) true,
                    position: (sequence $ replicate columns randomBool) <#> \bools -> bools <> replicate columns true <> (bools <#> not),
                    customSize: true
                }

    sizeLimit state = case state^._board of
        CircleBoard -> SizeLimit   3 1 12 1
        Grid3Board -> SizeLimit 3 1 3 9
        RandomBoard -> SizeLimit 3 1 3 9
        _ -> SizeLimit 7 7 7 7

    computerMove _ = pure Nothing
    updateScore = updateScore' AlwaysShowWin

instance scoregame :: ScoreGame (Array Boolean) ExtState {from :: Int, to :: Int} where
    objective _ = Minimize
    scoreFn = length ∘ filter identity ∘ view _position
    scoreHash state = joinWith "-" [show (state^._board), show (state^._nbRows), show (state^._nbColumns)]
    isCustomGame state = state^._board == RandomBoard

setBoardA :: ∀effs. Board -> Action State (rng :: RNG | effs)
setBoardA board = newGame \state ->
    let st2 = state # _board .~ board in 
    case board of
        CircleBoard -> st2 # _nbRows .~ 6 # _nbColumns .~ 1
        Grid3Board -> st2 # _nbRows .~ 3 # _nbColumns .~ 5
        RandomBoard -> st2 # _nbRows .~ 3 # _nbColumns .~ 5
        _ -> st2 # _nbRows .~ 7 # _nbColumns .~ 7

toggleHelpA :: ∀effs. Action State effs
toggleHelpA = setState $ _help %~ \x -> (x + 1) `mod` 3