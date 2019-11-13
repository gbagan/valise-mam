module Game.Chocolat where
import MyPrelude
import Pha.Action ((🔍))
import Game (class CGame)
import Game.Core (init) as C
import Game.Chocolat.Model (State, istate) as M
import Game.Chocolat.View (view) as V

newtype State = State M.State
is :: Iso' State M.State
is = iso (\(State a) -> a) State

instance cgame :: CGame State where
    init = is 🔍 C.init
    view lens (State st) = V.view (lens ∘ is) st
    onKeyDown _ = pure unit

state :: State
state = State M.istate