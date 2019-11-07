module Game.Solitaire where
import MyPrelude
import Game (class CGame)
import Game.Core (init) as C
import Game.Solitaire.Model (State, istate) as M
import Game.Solitaire.View (view) as V

newtype State = State M.State
is :: Iso' State M.State
is = iso (\(State a) -> a) State

instance cgame :: CGame State where
    init (State st) = State <$> C.init st
    view lens (State st) = V.view (lens ∘ is) st
    onKeyDown _ = pure unit

state :: State
state = State M.istate