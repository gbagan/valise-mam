module Game.Solitaire where
import Prelude
import Data.Lens (Iso', iso)
import Lib.Random (runRnd)
import Pha.Action (noAction)
import Game (class CGame)
import Game.Core (init) as C
import Game.Solitaire.Model (State, state) as M
import Game.Solitaire.View (view) as V
infixr 9 compose as ∘

newtype State = State M.State
is :: Iso' State M.State
is = iso (\(State a) -> a) State

instance cgame :: CGame State where
    init (State st) = runRnd (C.init st) <#> State -- todo simplifier? 
    view lens (State st) = V.view (lens ∘ is) st
    onKeyDown = const noAction

state :: State
state = State M.state