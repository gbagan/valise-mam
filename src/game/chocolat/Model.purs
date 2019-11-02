module Game.Chocolat.Model where
import Prelude
import Data.Lens (Lens', lens, view, (^.))
import Data.Int.Bits ((.^.))
import Lib.Util ((..))
import Game.Core (class Game, class TwoPlayersGame, SizeLimit(..), State(..), computerMove', _position, _nbRows, _nbColumns)
infixr 9 compose as ∘

data Move = FromLeft Int | FromRight Int | FromTop Int | FromBottom Int

type Position = {left :: Int, top :: Int, right :: Int, bottom :: Int}

type Ext' = {
    soap :: {row :: Int, col :: Int}
}
newtype ExtState = Ext Ext'
type ChocolatState = State Position ExtState

_ext :: Lens' ChocolatState Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))

_soap :: Lens' ChocolatState {row :: Int, col :: Int}
_soap = _ext ∘ lens (_.soap) (_{soap = _})

instance chocolatGame :: Game {left :: Int, top :: Int, right :: Int, bottom :: Int} ExtState Move where
    play state move =
        let p = state^._position in
        case move of
            FromLeft x -> p{left = x}
            FromTop x -> p{top = x}
            FromRight x -> p{right = x}
            FromBottom x -> p{bottom = x}

    -- les coups proposées par la vue sont toujours valides
    canPlay _ _ = true

    isLevelFinished = view _position >>> \{left, right, top, bottom} -> left == right - 1 && top == bottom - 1

    initialPosition state = pure { left: 0, right: state^._nbColumns, top: 0, bottom: state^._nbRows }

    sizeLimit = const (SizeLimit 4 4 10 10)
    onNewGame x = pure x
    computerMove = computerMove'

instance chocolat2Game :: TwoPlayersGame {left :: Int, top :: Int, right :: Int, bottom :: Int} ExtState Move where
    isLosingPosition state =
        let {left, right, top, bottom} = state^._position
            {row, col} = state^._soap
        in (col - left) .^. (right - col - 1) .^. (row - top) .^. (bottom - row - 1) == 0

    possibleMoves state =
        let {left, right, top, bottom} = state^._position
            {row, col} = state^._soap
        in
        ((left + 1) .. col <#> FromLeft) <> ((col + 1) .. (right - 1) <#> FromRight)
        <> ((top + 1) .. row <#> FromTop) <> ((row + 1) .. (bottom - 1) <#> FromRight) 

{-
export default template({
    state: {
        rows: 6,
        columns: 7,
        mode: 'expert',
        soapMode: 0,
        cutter: null,
    },

    core: {
        newGame: state => sequence([
                state.soapMode <= 1 ? ralways(0) : rint(state.rows),
                state.soapMode === 0 ? ralways(0) : rint(state.columns)
            ]) |> rlift (soap => ({...state, soap})),
    actions: $ => ({
        showCutter: update('cutter'),
        setSoapMode: $.newGame('soapMode')
    }),

    computed: ({ cutter, position }) => ({
        cutter2: cutter && (
            cutter.col === position.left && { col: position.right, row: cutter.row }
            || cutter.col === position.right && { col: position.left, row: cutter.row }
            || cutter.row === position.top && { col: cutter.col, row: position.bottom }
            || { col: cutter.col, row: position.top }
        )
    })
});