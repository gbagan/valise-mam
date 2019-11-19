module Game.Effs (EFFS, LOCATION, Location, getLocation, GetLocation, GetPointer, POINTER, Position, getPointerPosition,
                  EVENTEFF, EventEff, preventDefault, releasePointerCapture, interpretEffects, module A) where
import MyPrelude
import Effect (Effect)
import Lib.Random (genSeed)
import Run (FProxy, Run, SProxy(..), lift, match)
import Pha (InterpretEffs)
import Pha.Action (DELAY, Delay(..), RNG, Rng(..), GetEvent(..), getEvent, EVENT, delay, getState, setState, setState', Event) as A
import Pha.Event (preventDefault) as E

type Location = {
    hash :: String,
    host :: String,
    hostname :: String,
    href :: String,
    origin :: String,
    pathnme :: String,
    search :: String,
    username :: String,
    password :: String,
    origin :: String
}

data GetLocation a = GetLocation (Location -> a)
derive instance functorLoc :: Functor GetLocation
type LOCATION = FProxy GetLocation
getLocation :: ∀r. Run (location :: LOCATION | r) Location
getLocation = lift (SProxy :: SProxy "location") (GetLocation identity)

type Position = {x :: Number, y :: Number}

data GetPointer a = GetPointer (Maybe Position -> a)
derive instance functorPointer :: Functor GetPointer
type POINTER = FProxy GetPointer
getPointerPosition :: ∀r. Run (pointer :: POINTER | r) (Maybe Position)
getPointerPosition = lift (SProxy :: SProxy "pointer") (GetPointer identity)

data EventEffect = PreventDefault | ReleasePointerCapture

data EventEff a = EventEff EventEffect a
derive instance functorEvEff :: Functor EventEff
type EVENTEFF = FProxy EventEff
eventEff :: ∀r. EventEffect -> Run (eventEff :: EVENTEFF | r) Unit
eventEff e = lift (SProxy :: SProxy "eventEff") (EventEff e unit)

preventDefault :: ∀r. Run (eventEff :: EVENTEFF | r) Unit
preventDefault = eventEff PreventDefault
releasePointerCapture :: ∀r. Run (eventEff :: EVENTEFF | r) Unit
releasePointerCapture = eventEff ReleasePointerCapture

type EFFS = (rng :: A.RNG, delay :: A.DELAY, event :: A.EVENT, location :: LOCATION, pointer :: POINTER, eventEff :: EVENTEFF)

foreign import getLoc :: Effect Location
foreign import setTimeout :: Int -> Effect Unit -> Effect Unit

foreign import relativePointerPositionAux :: Maybe Position -> (Position -> Maybe Position) 
                                            -> A.Event -> Effect (Maybe Position)
relativePointerPosition :: A.Event -> Effect (Maybe Position)
relativePointerPosition = relativePointerPositionAux Nothing Just

foreign import releasePointerCaptureAux :: A.Event -> Effect Unit

interpretEffects :: A.Event -> InterpretEffs EFFS
interpretEffects ev = match {
    delay: \(A.Delay ms cont) -> setTimeout ms cont,
    rng: \(A.Rng cont) -> genSeed >>= cont,
    event: \(A.GetEvent cont) -> cont ev,
    location: \(GetLocation cont) -> getLoc >>= cont,
    pointer: \(GetPointer cont) -> relativePointerPosition ev >>= cont,
    eventEff: \(EventEff eff cont) -> case eff of
        PreventDefault -> E.preventDefault ev *> cont
        ReleasePointerCapture -> releasePointerCaptureAux ev *> cont
}