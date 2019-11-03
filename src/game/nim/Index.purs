module Game.Nim where
import Prelude
import Data.Lens (Iso', iso)
import Lib.Random (runRnd)
import Game (class CGame)
import Game.Core (init) as C
import Game.Nim.Model (State, istate) as M
import Game.Nim.View (view) as V
infixr 9 compose as ∘

newtype State = State M.State
is :: Iso' State M.State
is = iso (\(State a) -> a) State

instance cgame :: CGame State where
    init (State st) = runRnd (C.init st) <#> State -- todo simplifier? 
    view lens (State st) = V.view (lens ∘ is) st
    onKeyDown _ = mempty
state :: State
state = State M.istate