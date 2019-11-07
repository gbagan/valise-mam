module Pha.Action where
import MyPrelude
import Run (FProxy, Run(Run), SProxy(..), lift, on)
import Control.Monad.Free (hoistFree)
import Data.Functor.Variant (VariantF, inj)
import Unsafe.Coerce (unsafeCoerce)
import Lib.Random (Random, Seed, runRnd)

foreign import data Event :: Type

data LogF a = Log String a
derive instance functorLogF :: Functor LogF
type LOG = FProxy LogF
_log = SProxy :: SProxy "log"
log :: forall r. String -> Run (log :: LOG | r) Unit
log str = lift _log (Log str unit)

data DelayF a = Delay Int a
derive instance functorDelayF :: Functor DelayF
type DELAY = FProxy DelayF
_delay = SProxy :: SProxy "delay"
delay :: forall r. Int -> Run (delay :: DELAY | r) Unit
delay ms = lift _delay (Delay ms unit)

data GetStateF st a = GetState (st -> a)
derive instance functorGetStateF :: Functor (GetStateF st)
type GETSTATE st = FProxy (GetStateF st)
_getState = SProxy :: SProxy "getState"
getState :: forall st r. Run (getState :: GETSTATE st | r) st
getState = lift _getState (GetState identity)

data SetStateF st a = SetState (st -> st) a
derive instance functorSetStateF :: Functor (SetStateF st)
type SETSTATE st = FProxy (SetStateF st)
_setState = SProxy :: SProxy "setState"
setState :: forall st r. (st -> st) -> Run (setState :: SETSTATE st | r) Unit
setState fn = lift _setState (SetState fn unit)

data RngF a = Rng (Seed -> a)
derive instance functorRandF :: Functor RngF
type RNG = FProxy RngF
_rng = SProxy :: SProxy "rng"
rng :: forall r. Run (rng :: RNG | r) Seed
rng = lift _rng (Rng identity)

data GetEventF a = GetEvent (Event -> a)
derive instance functorEvF :: Functor GetEventF
type EVENT = FProxy GetEventF
_getEvent = SProxy :: SProxy "event"
getEvent :: forall r. Run (event :: EVENT | r) Event
getEvent = lift _getEvent (GetEvent identity)

type Action st effs = Run (getState :: GETSTATE st, setState :: SETSTATE st | effs) Unit

action :: forall effs st. (st -> st) -> Action st effs
action fn = setState fn

setState' :: forall effs st. (st -> st) -> Run (getState :: GETSTATE st, setState :: SETSTATE st | effs) st
setState' fn = do
    setState fn
    getState

randomAction :: forall effs st. (st -> Random st) -> Action st (rng :: RNG | effs)
randomAction fn = do
    st <- getState
    st2 <- rng <#> \x -> runRnd x (fn st)
    setState (\_ -> st2)

randomAction' :: forall effs st. (st -> Random st) ->  Run (getState :: GETSTATE st, setState :: SETSTATE st, rng :: RNG | effs) st
randomAction' fn = do
    randomAction fn
    getState

lensVariant :: forall st1 st2 v. Lens' st1 st2 -> VariantF (getState :: GETSTATE st2, setState :: SETSTATE st2 | v) ~>
                                                   VariantF (getState :: GETSTATE st1, setState :: SETSTATE st1 | v)
lensVariant lens variant = 
    variant # on _getState (\(GetState cont) -> inj _getState (GetState $ cont <<< view lens))
            (on _setState (\(SetState fn cont) -> inj _setState (SetState (lens %~ fn) cont)) unsafeCoerce)
    

zoomAt :: forall st1 st2 effs. Lens' st1 st2 -> Action st2 effs -> Action st1 effs
zoomAt lens (Run f) = Run $ hoistFree (lensVariant lens) f

infix 3 zoomAt as 🔍

{-
data FnList' a c b = Identity | Comp (a -> b) (FnList b c)
data FnList a c
mkListFn :: forall a b c. FnList' a c b -> FnList a c 
mkListFn = unsafeCoerce

unFnList :: forall a c r. (forall b. FnList' a c b -> r) -> FnList a c -> r
unFnList = unsafeCoerce
-}

{-
data Action' st sub a = SimpleAction (st -> st) | Action' (st -> {state :: st, sub :: sub a, cont :: a -> Action st})
data Action st

mkAction :: forall st sub a. Action' st sub a -> Action st
mkAction = unsafeCoerce

unAction :: forall st r. (forall a sub. Action' st sub a -> r) -> Action st -> r
unAction = unsafeCoerce

action :: forall st. (st -> st) -> Action st
action fn = mkAction (SimpleAction fn)



infixl 3  lensAction as 🔍
-}

-- data Async val st subs = Async (a -> state -> state)
{-
action :: forall st subs. (st -> st) -> Action st subs
action fn = Action \st -> {state: fn st, effects: []}

lensAction :: forall subs a b. Lens' a b -> Action b subs -> Action a subs
lensAction lens (Action fn) = Action \st -> 
    let {state, effects} = fn (st ^. lens) in
    {state: st # lens .~ state, effects: effects <#> \(Tuple sub fn2) -> Tuple sub (lensAction lens fn2)}



--newtype Action a = Action (((a -> a) -> Effect a) -> Event -> Aff a)

-instance semigroupAction :: Semigroup (Action a) where
    append (Action f) (Action g) = Action \dispatch e -> do
        _ <- f dispatch e
        g dispatch e

instance monoidAction :: Monoid (Action a) where
    mempty = Action (\dispatch ev -> liftEffect (dispatch identity))


action :: forall a. (a -> a) -> Action a
action fn = Action $ \dispatch e -> liftEffect $ dispatch fn

randomAction :: forall a. (a -> Random a) -> Action a
randomAction fn = Action $ \dispatch ev -> liftEffect do
    st <- dispatch identity
    st2 <- runRnd (fn st)
    dispatch (const st2)

type AsyncHelpers a = {
    getState :: Aff a,
    setState :: a -> Aff a,
    updateState :: (a -> a) -> Aff a,
    dispatch :: Action a -> Aff a
}

asyncAction :: forall a. (AsyncHelpers a -> a -> Aff a) -> Action a
asyncAction act = Action \dispatch e -> do
    st <- liftEffect (dispatch identity)
    act {
        getState: liftEffect $ dispatch identity,
        setState: \x -> liftEffect $ dispatch (const x),
        updateState: \fn -> liftEffect $ dispatch fn,
        dispatch: \(Action a) -> a dispatch e
    } st

ifThenElseA :: forall a. (a -> Event -> Boolean) -> Action a -> Action a -> Action a
ifThenElseA cond (Action action1) (Action action2) = Action $ \dispatch ev -> do
    st <- liftEffect $ dispatch identity
    (if cond st ev then action1 else action2) dispatch ev

whenA :: forall a. (a -> Event -> Boolean) -> Action a -> Action a
whenA cond act = ifThenElseA cond act mempty

withPayload :: forall a b. (b -> Action a) -> (Event -> b) -> Action a
withPayload act payloadFn = Action \dispatch e -> do
    let (Action a) = act (payloadFn e)
    a dispatch e

withPayload' :: forall a b. (b -> Action a) -> (Event -> Effect b) -> Action a
withPayload' act payloadFn = Action \dispatch e -> do
    payload <- liftEffect $ payloadFn e
    let (Action a) = act payload
    a dispatch e

onlyEffectAction :: forall a. (Event -> Effect Unit) -> Action a
onlyEffectAction eff = Action \dispatch e -> do
    liftEffect (eff e)
    liftEffect (dispatch identity)