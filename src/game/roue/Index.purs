module Game.Roue (State, state) where
import MyPrelude
import Game (class CGame)
import Game.Core (init) as C
import Game.Roue.Model (State, istate) as M
import Game.Roue.View (view) as V

newtype State = State M.State
_iso :: Iso' State M.State
_iso = iso (\(State a) -> a) State

instance cgame :: CGame State where
    init (State st) = State <$> C.init st
    view lens (State st) = V.view (lens ∘ _iso) st
    onKeyDown _ = pure unit

state :: State
state = State M.istate