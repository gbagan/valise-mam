module Game.Nim.Model where
import MyPrelude
import Data.Int.Bits (xor)
import Lib.Util (tabulate2)
import Pha.Action (Action)
import Pha.Effects.Random (randomInt)
import Game.Effs (EFFS)
import Game.Core (class Game, class TwoPlayersGame, GState, Mode(..), Turn(..),
                _ext, genState, newGame', _position, _turn, computerMove', defaultSizeLimit, defaultOnNewGame)

data Move = Move Int Int  --- pile et position dans la pile
type Ext' = { 
    nbPiles :: Int,
    length :: Int   ---- longueur d'une rangée
}
newtype ExtState = Ext Ext'
type State = GState (Array (Tuple Int Int)) ExtState

-- état initial
istate :: State
istate = genState [] _{mode = ExpertMode } (Ext { length: 10, nbPiles: 4 })

-- lenses
_ext' :: Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) -> a) Ext
_length :: Lens' State Int
_length = _ext' ∘ lens _.length _{length = _}
_nbPiles :: Lens' State Int
_nbPiles = _ext' ∘ lens _.nbPiles _{nbPiles = _}

canPlay :: State -> Move -> Boolean
canPlay state (Move pile pos) =
    case state^._position !! pile of
        Nothing -> false 
        Just (p1 ∧ p2) -> pos /= p1 && pos /= p2 && if state^._turn == Turn1 then pos < p2 else pos > p1

instance nimGame :: Game (Array (Tuple Int Int)) ExtState Move where
    play state move@(Move pile pos) = 
        if canPlay state move then
            state^._position # modifyAt pile
                \(p1 ∧ p2) -> if state^._turn == Turn1 then pos ∧ p2 else p1 ∧ pos
        else
            Nothing
    
    isLevelFinished state = state^._position # all
        \(p1 ∧ p2) -> p2 - p1 == 1 && p1 == (if state^._turn == Turn2 then state^._length - 2 else 0)

    initialPosition state = 
        sequence $ replicate (state^._nbPiles) $
            if state^._length == 5 then
                pure (0 ∧ 4)
            else do 
                x <- randomInt 5
                y <- randomInt 5
                pure (x ∧ (y + 5))

    computerMove = computerMove'
    sizeLimit = defaultSizeLimit
    onNewGame = defaultOnNewGame
    updateScore st = st ∧ true

instance nimGame2 :: TwoPlayersGame (Array (Tuple Int Int)) ExtState Move where
    possibleMoves state =
        tabulate2 (state^._nbPiles) (state^._length) Move
        # filter (canPlay state)
        # sortWith \(Move pile pos) -> state^._position !! pile # maybe 0
            \x -> if state^._turn == Turn1 then fst x - pos else pos - snd x

    isLosingPosition = eq 0 ∘ foldr (\t -> xor (snd t - fst t - 1)) 0 ∘ view _position

setNbPilesA :: Int -> Action State EFFS
setNbPilesA = newGame' (set _nbPiles)

setLengthA :: Int -> Action State EFFS
setLengthA = newGame'(set _length)
