module Game.Nim.Model where
import MyPrelude
import Data.Int.Bits (xor)
import Lib.Util (repeat2)
import Pha.Update (Update)
import Pha.Random as R
import Game.Effs (EFFS)
import Game.Core (class Game, class TwoPlayersGame, class MsgWithCore, CoreMsg, GState, Mode(..), Turn(..),
            coreUpdate, playA,
            _ext, genState, newGame, _position, _turn, computerMove', defaultSizeLimit, defaultOnNewGame)

-- une position donne pour chaque numéro de rangée une paire indiquant la position de chaque jetons
-- un coup (move) est du type Move i j où i est le numéro de pile et j la position dans la pile

data Move = Move Int Int -- pile et position dans la pile
type Ext' = 
    {   nbPiles ∷ Int    -- nombre de rangées
    ,   length ∷ Int     -- nombre de cases sur une rangée
    }
newtype ExtState = Ext Ext'
type State = GState (Array (Tuple Int Int)) ExtState

-- état initial
istate ∷ State
istate = genState [] _{mode = ExpertMode} (Ext {length: 10, nbPiles: 4})

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_length ∷ Lens' State Int
_length = _ext' ∘ lens _.length _{length = _}
_nbPiles ∷ Lens' State Int
_nbPiles = _ext' ∘ lens _.nbPiles _{nbPiles = _}

canPlay ∷ State → Move → Boolean
canPlay state (Move pile pos) =
    case state^._position !! pile of
        Nothing → false 
        Just (p1 ∧ p2) → pos /= p1 && pos /= p2 && if state^._turn == Turn1 then pos < p2 else pos > p1

instance nimGame ∷ Game (Array (Tuple Int Int)) ExtState Move where
    play state move@(Move pile pos) = 
        if canPlay state move then
            state^._position # modifyAt pile
                \(p1 ∧ p2) → if state^._turn == Turn1 then pos ∧ p2 else p1 ∧ pos
        else
            Nothing
    
    isLevelFinished state = state^._position # all
        \(p1 ∧ p2) → p2 - p1 == 1 && p1 == (if state^._turn == Turn2 then state^._length - 2 else 0)

    initialPosition state = 
        replicateA (state^._nbPiles) $
            if state^._length == 5 then
                pure (0 ∧ 4)
            else do 
                Tuple <$> (R.int 0 4) <*> (R.int 0 9)

    computerMove = computerMove'

    -- fonctions par défault
    sizeLimit = defaultSizeLimit
    onNewGame = defaultOnNewGame
    updateScore st = st ∧ true
    onPositionChange = identity

instance nimGame2 ∷ TwoPlayersGame (Array (Tuple Int Int)) ExtState Move where
    possibleMoves state =
        repeat2 (state^._nbPiles) (state^._length) Move
        # filter (canPlay state)
        # sortWith \(Move pile pos) → state^._position !! pile # maybe 0
            \x → if state^._turn == Turn1 then fst x - pos else pos - snd x

    isLosingPosition = view _position >>> foldr (\t → xor (snd t - fst t - 1)) 0 >>> (_ == 0)

data Msg = Core CoreMsg | SetNbPiles Int | SetLength Int | Play Move
instance withcore ∷ MsgWithCore Msg where core = Core

update ∷ Msg → Update State EFFS
update (Core msg) = coreUpdate msg
update (SetNbPiles n) = newGame $ _nbPiles .~ n
update (SetLength n) = newGame $ _length .~ n
update (Play n) = playA n