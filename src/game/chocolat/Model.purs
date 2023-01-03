module Game.Chocolat.Model where

import MamPrelude
import Data.Int.Bits ((.^.))
import Game.Core (class Game, class TwoPlayersGame, class MsgWithCore, CoreMsg, SizeLimit(..), GState, Mode(..), 
                  coreUpdate, playA, _ext, genState, newGame, computerMove', _position, _nbRows, _nbColumns, defaultUpdateScore)
import Lib.Util (chooseInt')
import Lib.Update (UpdateMam)

data Move = FromLeft Int | FromRight Int | FromTop Int | FromBottom Int
data SoapMode = CornerMode | BorderMode | StandardMode | CustomMode
derive instance eqSoapMode ∷ Eq SoapMode

type Position = {left ∷ Int, top ∷ Int, right ∷ Int, bottom ∷ Int}

type Ext' =
    {   soap ∷ Maybe {row ∷ Int, col ∷ Int}
    ,   soapMode ∷ SoapMode
    ,   moveWhenHover ∷ Maybe Move
    }

newtype ExtState = Ext Ext'
type State = GState Position ExtState

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_soap ∷ Lens' State (Maybe {row ∷ Int, col ∷ Int})
_soap = _ext' ∘ prop (Proxy ∷ _ "soap")
_soapMode ∷ Lens' State SoapMode
_soapMode = _ext' ∘ prop (Proxy ∷ _ "soapMode")
_moveWhenHover ∷ Lens' State (Maybe Move)
_moveWhenHover = _ext' ∘ prop (Proxy ∷ _ "moveWhenHover")

-- | état initial
istate ∷ State
istate = genState {left: 0, top: 0, right: 0, bottom: 0} _{nbRows = 6, nbColumns = 7, mode = RandomMode}
        (Ext { soap: Just {row: 0, col: 0}, soapMode: CornerMode, moveWhenHover: Nothing})

instance Game Position ExtState Move where
    name _ = "chocolat"

    -- les coups proposées par la vue sont toujours valides
    play st m = Just case m  of
            FromLeft x → p{left = x}
            FromTop x → p{top = x}
            FromRight x → p{right = x}
            FromBottom x → p{bottom = x}
        where p = st^._position

    isLevelFinished = view _position >>> \p → p.left == p.right - 1 && p.top == p.bottom - 1

    initialPosition st = pure { left: 0, right: st^._nbColumns, top: 0, bottom: st^._nbRows }

    onNewGame state = 
        if state^._soapMode == CustomMode then
            pure $ state # set _soap Nothing
        else do
            row ← if state^._soapMode == StandardMode then chooseInt' (state^._nbRows) else pure 0
            col ← if state^._soapMode ≠ CornerMode then chooseInt' (state^._nbColumns) else pure 0
            pure $ state # set _soap (Just {row, col})

    sizeLimit _ = SizeLimit 4 4 10 10
    computerMove = computerMove'

    -- fonctions par défaut
    onPositionChange = identity
    updateScore s = defaultUpdateScore s
    saveToJson _ = Nothing
    loadFromJson st _ = st

instance TwoPlayersGame {left ∷ Int, top ∷ Int, right ∷ Int, bottom ∷ Int} ExtState Move where
    isLosingPosition st =
        case st^._soap of
            Just {row, col} → 
                (col - left) .^. (right - col - 1) .^. (row - top) .^. (bottom - row - 1) == 0 where
                {left, right, top, bottom} = st^._position
            Nothing → false

    possibleMoves st =
        case st^._soap of
            Just {row, col} → 
                let {left, right, top, bottom} = st^._position in
                (FromLeft <$> (left + 1) .. col) <> (FromRight <$> (col + 1) .. (right - 1))
                <> (FromTop <$> (top + 1) .. row) <> (FromBottom <$> (row + 1) .. (bottom - 1)) 
            Nothing → []

cutLine ∷ State → Move → {x1 ∷ Int, x2 ∷ Int, y1 ∷ Int, y2 ∷ Int}
cutLine state = case _ of
    FromLeft i → {x1: i, y1: top, x2: i, y2: bottom}
    FromRight i → {x1: i, y1: top, x2: i, y2: bottom}
    FromTop i → {x1: left, y1: i, x2: right, y2: i}
    FromBottom i → {x1: left, y1: i, x2: right, y2: i}
    where {left, right, top, bottom} = state^._position

data Msg = Core CoreMsg 
        | SetHover (Maybe Move)
        | SetSoapMode SoapMode 
        | Play Move
        | SetSoap Int Int
        | NoAction
instance MsgWithCore Msg where core = Core
    
update ∷ Msg → UpdateMam State Unit
update (Core msg) = coreUpdate msg
update (SetHover a) = _moveWhenHover .= a 
update (SetSoapMode m) = newGame $ set _soapMode m
update (Play move) = (_moveWhenHover .= Nothing) *> playA move
update (SetSoap row col) = _soap .= Just {row, col}
update NoAction = pure unit