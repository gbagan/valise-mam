module Game.Frog where
import MyPrelude
import Pha.Action ((🎲))
import Game (class CGame)
import Game.Core (init) as C
import Game.Frog.Model (State, istate, onKeyDown) as M
import Game.Frog.View (view) as V

newtype State = State M.State
is :: Iso' State M.State
is = iso (\(State a) -> a) State

instance cgame :: CGame State where
    init (State st) = State <$> C.init st
    view lens (State st) = V.view (lens ∘ is) st
    onKeyDown a = is 🎲 M.onKeyDown a

state :: State
state = State M.istate