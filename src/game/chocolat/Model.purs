module Game.Chocolat.Model where
import MyPrelude
import Data.Int.Bits ((.^.))
import Lib.Util ((..))
import Lib.Random (randomInt)
import Pha.Action (Action, RNG, setState)
import Game.Core (class Game, class TwoPlayersGame, SizeLimit(..), GState, Mode(..),
                   _ext, genState, newGame', computerMove', _position, _nbRows, _nbColumns)

data Move = FromLeft Int | FromRight Int | FromTop Int | FromBottom Int
data SoapMode = CornerMode | BorderMode | StandardMode
derive instance eqSoapMode :: Eq SoapMode
instance showSoapMode :: Show SoapMode where show _ = ""

type Position = {left :: Int, top :: Int, right :: Int, bottom :: Int}

type Ext' = {
    soap :: {row :: Int, col :: Int},
    soapMode :: SoapMode,
    moveWhenHover :: Maybe Move
}
newtype ExtState = Ext Ext'
type State = GState Position ExtState

-- lenses
_ext' :: Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) -> a) Ext
_soap :: Lens' State {row :: Int, col :: Int}
_soap = _ext' ∘ lens _.soap _{soap = _}
_soapMode :: Lens' State SoapMode
_soapMode = _ext' ∘ lens _.soapMode _{soapMode = _}
_moveWhenHover :: Lens' State (Maybe Move)
_moveWhenHover = _ext' ∘ lens _.moveWhenHover _{moveWhenHover = _}

-- état initial
istate :: State
istate = genState {left: 0, top: 0, right: 0, bottom: 0} _{nbRows = 6, nbColumns = 7, mode = RandomMode}
        (Ext { soap: {row: 0, col: 0}, soapMode: CornerMode, moveWhenHover: Nothing})

instance gameChocolat :: Game {left :: Int, top :: Int, right :: Int, bottom :: Int} ExtState Move where
    play st = case _ of
            FromLeft x -> p{left = x}
            FromTop x -> p{top = x}
            FromRight x -> p{right = x}
            FromBottom x -> p{bottom = x}
        where p = st^._position

    -- les coups proposées par la vue sont toujours valides
    canPlay _ _ = true

    isLevelFinished = view _position >>> \{left, right, top, bottom} -> left == right - 1 && top == bottom - 1

    initialPosition st = pure { left: 0, right: st^._nbColumns, top: 0, bottom: st^._nbRows }

    onNewGame state = do
        row <- if state^._soapMode == StandardMode then randomInt (state^._nbRows) else pure 0
        col <- if state^._soapMode /= CornerMode then randomInt (state^._nbColumns) else pure 0
        pure $ state # _soap .~ {row, col}

    sizeLimit = const (SizeLimit 4 4 10 10)
    computerMove = computerMove'
    updateScore st = st ∧ true

instance chocolat2Game :: TwoPlayersGame {left :: Int, top :: Int, right :: Int, bottom :: Int} ExtState Move where
    isLosingPosition st = (col - left) .^. (right - col - 1) .^. (row - top) .^. (bottom - row - 1) == 0 where
        {left, right, top, bottom} = st^._position
        {row, col} = st^._soap 

    possibleMoves st =
        let {left, right, top, bottom} = st^._position
            {row, col} = st^._soap
        in
        ((left + 1) .. col <#> FromLeft) <> ((col + 1) .. (right - 1) <#> FromRight)
        <> ((top + 1) .. row <#> FromTop) <> ((row + 1) .. (bottom - 1) <#> FromBottom) 

cutLine :: State -> Move -> {x1 :: Int, x2 :: Int, y1 :: Int, y2 :: Int}
cutLine state = case _ of
    FromLeft i -> {x1: i, y1: top, x2: i, y2: bottom}
    FromRight i -> {x1: i, y1: top, x2: i, y2: bottom}
    FromTop i -> {x1: left, y1: i, x2: right, y2: i}
    FromBottom i -> {x1: left, y1: i, x2: right, y2: i}
    where {left, right, top, bottom} = state^._position

setHoverA :: ∀effs. Maybe Move -> Action State effs
setHoverA a = setState (_moveWhenHover .~ a) 

setSoapModeA :: ∀effs. SoapMode -> Action State (rng :: RNG | effs)
setSoapModeA = newGame' (set _soapMode)
