module Pha.Action where
import MyPrelude
import Run (FProxy, Run(Run), SProxy(..), lift, onMatch)
import Control.Monad.Free (hoistFree)
import Data.Functor.Variant (VariantF, inj)
import Unsafe.Coerce (unsafeCoerce)
import Lib.Random (Random, Seed, runRnd)

foreign import data Event :: Type

data GetStateF st a = GetState (st -> a)
derive instance functorGetStateF :: Functor (GetStateF st)
type GETSTATE st = FProxy (GetStateF st)
getState :: ∀st r. Run (getState :: GETSTATE st | r) st
getState = lift (SProxy :: SProxy "getState") (GetState identity)

data SetStateF st a = SetState (st -> st) a
derive instance functorSetStateF :: Functor (SetStateF st)
type SETSTATE st = FProxy (SetStateF st)
setState :: ∀st r. (st -> st) -> Run (setState :: SETSTATE st | r) Unit
setState fn = lift (SProxy :: SProxy "setState") (SetState fn unit)

data GetEventF a = GetEvent (Event -> a)
derive instance functorEvF :: Functor GetEventF
type EVENT = FProxy GetEventF
getEvent :: ∀r. Run (event :: EVENT | r) Event
getEvent = lift (SProxy :: SProxy "event") (GetEvent identity)

type Action' st effs a = Run (getState :: GETSTATE st, setState :: SETSTATE st | effs) a
type Action st effs = Action' st effs Unit

setState' :: ∀effs st. (st -> st) -> Run (getState :: GETSTATE st, setState :: SETSTATE st | effs) st
setState' fn = do
    setState fn
    getState


lensVariant :: ∀st1 st2 v. Lens' st1 st2 -> VariantF (getState :: GETSTATE st2, setState :: SETSTATE st2 | v) ~>
                                                   VariantF (getState :: GETSTATE st1, setState :: SETSTATE st1 | v)
lensVariant lens = 
    onMatch {
        getState: \(GetState cont) -> inj (SProxy :: SProxy "getState") (GetState (cont ∘ view lens)),
        setState: \(SetState fn cont) -> inj (SProxy :: SProxy "setState") (SetState (lens %~ fn) cont)
    } unsafeCoerce
    

zoomAt :: ∀st1 st2 effs. Lens' st1 st2 -> Action st2 effs -> Action st1 effs
zoomAt lens (Run f) = Run $ hoistFree (lensVariant lens) f

infix 3 zoomAt as 🔍



data RngF a = Rng (Seed -> a)
derive instance functorRandF :: Functor RngF
type RNG = FProxy RngF
rng :: ∀r. Run (rng :: RNG | r) Seed
rng = lift (SProxy :: SProxy "rng") (Rng identity)

data DelayF a = Delay Int a
derive instance functorDelayF :: Functor DelayF
type DELAY = FProxy DelayF
delay :: ∀r. Int -> Run (delay :: DELAY | r) Unit
delay ms = lift (SProxy :: SProxy "delay") (Delay ms unit)

randomAction :: ∀effs st. (st -> Random st) -> Action st (rng :: RNG | effs)
randomAction fn = do
    st <- getState
    st2 <- rng <#> \x -> runRnd x (fn st)
    setState (\_ -> st2)

randomAction' :: ∀effs st. (st -> Random st) ->  Run (getState :: GETSTATE st, setState :: SETSTATE st, rng :: RNG | effs) st
randomAction' fn = do
    randomAction fn
    getState