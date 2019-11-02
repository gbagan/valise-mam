module Game.Valise where
import Prelude
import Data.Lens (Iso', iso)
import Pha.Action (noAction)
import Game (class CGame)
import Game.Valise.Model (State, state) as M
import Game.Valise.View (view) as V
infixr 9 compose as ∘

newtype State = State M.State
is :: Iso' State M.State
is = iso (\(State a) -> a) State

instance cgame :: CGame State where
    init st = pure st -- runRnd (C.init st) <#> State -- todo simplifier? 
    view lens (State st) = V.view (lens ∘ is) st
    onKeyDown = const noAction

state :: State
state = State M.state