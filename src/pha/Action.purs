module Pha.Action (getState, setState, setState', GetState(..), SetState(..), GETSTATE, SETSTATE, 
    delay, Delay(..), DELAY, Action, Action', Event, EVENT, getEvent, GetEvent(..), module R) where
import MyPrelude
import Run (FProxy, Run, SProxy(..), lift)
import Pha.Random (RNG) as R

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
setState' :: ∀effs st. (st -> st) -> Run (getState :: GETSTATE st, setState :: SETSTATE st | effs) st
setState' fn = setState fn *> getState

data GetEvent a = GetEvent (Event -> a)
derive instance functorEvF :: Functor GetEvent
type EVENT = FProxy GetEvent

getEvent :: ∀r. Run (event :: EVENT | r) Event
getEvent = lift (SProxy :: SProxy "event") (GetEvent identity)

type Action' st effs a = Run (getState :: GETSTATE st, setState :: SETSTATE st | effs) a
type Action st effs = Action' st effs Unit

data Delay a = Delay Int a
derive instance functorDelay :: Functor Delay
type DELAY = FProxy Delay

delay :: ∀r. Int -> Run (delay :: DELAY | r) Unit
delay ms = lift (SProxy :: SProxy "delay") (Delay ms unit)
