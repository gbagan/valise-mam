module Game.Nim (State, state) where
import MyPrelude
import Pha.Lens (actionOver, viewOver)
import Game (class CGame)
import Game.Core (init) as C
import Game.Nim.Model (State, istate) as M
import Game.Nim.View (view) as V

newtype State = State M.State
_iso :: Iso' State M.State
_iso = iso (\(State a) -> a) State

instance cgame :: CGame State where
    init = actionOver _iso C.init
    view lens (State st) = viewOver (lens ∘ _iso) (V.view st)
    onKeyDown _ = pure unit
state :: State
state = State M.istate