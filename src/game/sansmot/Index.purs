module Game.Sansmot (State, state) where
import MyPrelude
import Pha.Lens (viewOver)
import Game (class CGame)
import Game.Sansmot.Model (State, istate) as M
import Game.Sansmot.View (view) as V

newtype State = State M.State
_iso :: Iso' State M.State
_iso = iso (\(State a) -> a) State

instance cgame :: CGame State where
    init = pure unit
    view lens (State st) = viewOver (lens ∘ _iso) (V.view st)
    onKeyDown _ = pure unit

state :: State
state = State M.istate