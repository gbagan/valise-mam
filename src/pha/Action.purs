module Pha.Action where
import MyPrelude
import Run (FProxy, Run, SProxy(..), lift)
import Lib.Random (Random, Seed, runRnd)

foreign import data Event :: Type

data GetState st a = GetState (st -> a)
derive instance functorGetState :: Functor (GetState st)
type GETSTATE st = FProxy (GetState st)
getState :: ∀st r. Run (getState :: GETSTATE st | r) st
getState = lift (SProxy :: SProxy "getState") (GetState identity)

data SetState st a = SetState (st -> st) a
derive instance functorSetState :: Functor (SetState st)
type SETSTATE st = FProxy (SetState st)
setState :: ∀st r. (st -> st) -> Run (setState :: SETSTATE st | r) Unit
setState fn = lift (SProxy :: SProxy "setState") (SetState fn unit)

data GetEvent a = GetEvent (Event -> a)
derive instance functorEvF :: Functor GetEvent
type EVENT = FProxy GetEvent
getEvent :: ∀r. Run (event :: EVENT | r) Event
getEvent = lift (SProxy :: SProxy "event") (GetEvent identity)

type Action' st effs a = Run (getState :: GETSTATE st, setState :: SETSTATE st | effs) a
type Action st effs = Action' st effs Unit

setState' :: ∀effs st. (st -> st) -> Run (getState :: GETSTATE st, setState :: SETSTATE st | effs) st
setState' fn = do
    setState fn
    getState


data Rng a = Rng (Seed -> a)
derive instance functorRng :: Functor Rng
type RNG = FProxy Rng
rng :: ∀r. Run (rng :: RNG | r) Seed
rng = lift (SProxy :: SProxy "rng") (Rng identity)

data Delay a = Delay Int a
derive instance functorDelay :: Functor Delay
type DELAY = FProxy Delay
delay :: ∀r. Int -> Run (delay :: DELAY | r) Unit
delay ms = lift (SProxy :: SProxy "delay") (Delay ms unit)

runRng :: ∀effs st a. (Random a) -> Action' st (rng :: RNG | effs) a
runRng a = rng <#> \x -> runRnd x a

randomAction :: ∀effs st. (st -> Random st) -> Action st (rng :: RNG | effs)
randomAction fn = do
    st <- getState
    st2 <- runRng (fn st)
    setState (\_ -> st2)

randomAction' :: ∀effs st. (st -> Random st) ->  Run (getState :: GETSTATE st, setState :: SETSTATE st, rng :: RNG | effs) st
randomAction' fn = do
    randomAction fn
    getState