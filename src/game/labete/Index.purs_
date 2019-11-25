module Game.Labete (State, state) where
import MyPrelude
import Pha.Lens (actionOver, viewOver)
import Game (class CGame)
import Game.Core (init) as C
import Game.Labete.Model (State, istate, onKeyDown) as M
import Game.Labete.View (view) as V

newtype State = State M.State
_iso :: Iso' State M.State
_iso = iso (\(State a) -> a) State

instance cgame :: CGame State where
    init = actionOver _iso C.init
    view lens (State st) = viewOver (lens ∘ _iso) (V.view st)
    onKeyDown a = actionOver _iso (M.onKeyDown a)

state :: State
state = State M.istate