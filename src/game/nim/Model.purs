module Game.Nim.Model where

import MamPrelude
import Control.Monad.Gen (chooseInt)
import Data.Int.Bits (xor)
import Game.Core (class Game, class TwoPlayersGame, class MsgWithCore, CoreMsg, GModel, Mode(..), Turn(..),
        coreUpdate, playA, _ext, genModel, newGame, _position, _turn, computerMove',
        defaultSizeLimit, defaultOnNewGame, defaultUpdateScore)
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
newtype ExtModel = Ext Ext'
type Model = GModel (Array Position) ExtModel

-- état initial
imodel ∷ Model
imodel = genModel [] _{mode = ExpertMode} (Ext {length: 10, nbPiles: 4})

-- lenses
_ext' ∷ Lens' Model Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_length ∷ Lens' Model Int
_length = _ext' ∘ prop (Proxy ∷ _ "length")
_nbPiles ∷ Lens' Model Int
_nbPiles = _ext' ∘ prop (Proxy ∷ _ "nbPiles")

canPlay ∷ Model → Move → Boolean
canPlay model (Move pile pos) =
    case model^._position !! pile of
        Nothing → false
        Just (Position p1 p2) → pos ≠ p1 
                             && pos ≠ p2
                             && if model^._turn == Turn1 then pos < p2 else pos > p1

instance Game (Array Position) ExtModel Move where
    name _ = "nim"
    
    play model move@(Move pile pos) = do
        guard $ canPlay model move
        model^._position # modifyAt pile
            \(Position p1 p2) → if model^._turn == Turn1 then Position pos p2 else Position p1 pos
    
    isLevelFinished model = model^._position # all
        \(Position p1 p2) → p2 - p1 == 1 && p1 == (if model^._turn == Turn2 then model^._length - 2 else 0)

    initialPosition model = 
        replicateA (model^._nbPiles) $
            if model^._length == 5 then
                pure (Position 0 4)
            else
                Position <$> chooseInt 0 4 <*> chooseInt 5 9

    computerMove model = computerMove' model

    -- fonctions par défault
    sizeLimit = defaultSizeLimit
    onNewGame = defaultOnNewGame
    updateScore s = defaultUpdateScore s
    onPositionChange = identity
    saveToJson _ = Nothing
    loadFromJson model _ = model

instance TwoPlayersGame (Array Position) ExtModel Move where
  possibleMoves model =
    repeat2 (model^._nbPiles) (model^._length) Move
    # filter (canPlay model)
    # sortWith \(Move pile pos) → case model ^. _position !! pile of
                                    Nothing → 0
                                    Just (Position x y) → if model ^. _turn == Turn1 then x - pos else pos - y

  isLosingPosition model = (model^._position # foldr (\(Position x y) → xor (y - x - 1)) 0) == 0

data Msg = Core CoreMsg | SetNbPiles Int | SetLength Int | Play Move
instance MsgWithCore Msg where core = Core

update ∷ Msg → UpdateMam Model Unit
update (Core msg) = coreUpdate msg
update (SetNbPiles n) = newGame $ set _nbPiles n
update (SetLength n) = newGame $ set _length n
update (Play n) = playA n