module Game.Nim.Model where

import MyPrelude
import Control.Monad.Gen (chooseInt)
import Data.Int.Bits (xor)
import Game.Core (class Game, class TwoPlayersGame, class MsgWithCore, CoreMsg, GState, Mode(..), Turn(..),
        coreUpdate, playA, _ext, genState, newGame, _position, _turn, computerMove', defaultSizeLimit, defaultOnNewGame)
import Lib.Update (UpdateMam)
import Lib.Util (repeat2)

-- une position donne pour chaque numéro de rangée une paire indiquant la position de chaque jetons
-- un coup (move) est du type Move i j où i est le numéro de pile et j la position dans la pile

data Position = Position Int Int
data Move = Move Int Int -- pile et position dans la pile

type Ext' = 
    {   nbPiles ∷ Int    -- nombre de rangées
    ,   length ∷ Int     -- nombre de cases sur une rangée
    }
newtype ExtState = Ext Ext'
type State = GState (Array Position) ExtState

-- état initial
istate ∷ State
istate = genState [] _{mode = ExpertMode} (Ext {length: 10, nbPiles: 4})

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_length ∷ Lens' State Int
_length = _ext' ∘ prop (Proxy ∷ _ "length")
_nbPiles ∷ Lens' State Int
_nbPiles = _ext' ∘ prop (Proxy ∷ _ "nbPiles")

canPlay ∷ State → Move → Boolean
canPlay state (Move pile pos) =
    case state^._position !! pile of
        Nothing → false
        Just (Position p1 p2) → pos ≠ p1 
                             && pos ≠ p2
                             && if state^._turn == Turn1 then pos < p2 else pos > p1

instance Game (Array Position) ExtState Move where
    name _ = "nim"
    
    play state move@(Move pile pos) = do
        guard $ canPlay state move
        state^._position # modifyAt pile
            \(Position p1 p2) → if state^._turn == Turn1 then Position pos p2 else Position p1 pos
    
    isLevelFinished state = state^._position # all
        \(Position p1 p2) → p2 - p1 == 1 && p1 == (if state^._turn == Turn2 then state^._length - 2 else 0)

    initialPosition state = 
        replicateA (state^._nbPiles) $
            if state^._length == 5 then
                pure (Position 0 4)
            else
                Position <$> chooseInt 0 4 <*> chooseInt 5 9

    computerMove = computerMove'

    -- fonctions par défault
    sizeLimit = defaultSizeLimit
    onNewGame = defaultOnNewGame
    updateScore st = st ∧ true
    onPositionChange = identity
    saveToJson _ = Nothing
    loadFromJson st _ = st

instance TwoPlayersGame (Array Position) ExtState Move where
    possibleMoves state =
        repeat2 (state^._nbPiles) (state^._length) Move
        # filter (canPlay state)
        # sortWith \(Move pile pos) → case state ^. _position !! pile of
                                        Nothing → 0
                                        Just (Position x y) → if state ^. _turn == Turn1 then x - pos else pos - y

    isLosingPosition st = (st ^. _position # foldr (\(Position x y) → xor (y - x - 1)) 0) == 0

data Msg = Core CoreMsg | SetNbPiles Int | SetLength Int | Play Move
instance MsgWithCore Msg where core = Core

update ∷ Msg → UpdateMam State
update (Core msg) = coreUpdate msg
update (SetNbPiles n) = newGame $ set _nbPiles n
update (SetLength n) = newGame $ set _length n
update (Play n) = playA n