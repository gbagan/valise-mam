module Game.Jetons (State, state) where
import MyPrelude
import Pha.Action ((🔍))
import Game (class CGame)
import Game.Core (init) as C
import Game.Jetons.Model (State, istate) as M
import Game.Jetons.View (view) as V

newtype State = State M.State
_iso :: Iso' State M.State
_iso = iso (\(State a) -> a) State

instance cgame :: CGame State where
    init = _iso 🔍 C.init
    view lens (State st) = V.view (lens ∘ _iso) st
    onKeyDown _ = pure unit
state :: State
state = State M.istate