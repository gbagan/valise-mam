module Game.Valise where
import MyPrelude
import Game (class CGame)
import Game.Valise.Model (State, istate) as M
import Game.Valise.View (view) as V

newtype State = State M.State
_iso :: Iso' State M.State
_iso = iso (\(State a) -> a) State

instance cgame :: CGame State where
    init = pure unit
    view lens (State st) = V.view (lens ∘ _iso) st
    onKeyDown _ = pure unit

state :: State
state = State M.istate