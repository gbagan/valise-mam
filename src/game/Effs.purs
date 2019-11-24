module Game.Effs (EFFS, LOCATION, Location, getLocation, GetLocation, GetPointer, POINTER, Position, getPointerPosition,
                  EVENTEFF, EventEff, releasePointerCapture, interpretEffects) where
import MyPrelude
import Effect (Effect)
import Run (FProxy, Run, SProxy(..), lift, match)
import Pha (InterpretEffs, Event)
import Pha.Event (EVENT, interpretEvent)
import Pha.Effects.Delay (DELAY, interpretDelay)
import Pha.Effects.Random (RNG, interpretRng)

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

data GetPointer a = GetPointer Event (Maybe Position -> a)
derive instance functorPointer :: Functor GetPointer
type POINTER = FProxy GetPointer
getPointerPosition :: ∀r. Event -> Run (pointer :: POINTER | r) (Maybe Position)
getPointerPosition ev = lift (SProxy :: SProxy "pointer") (GetPointer ev identity)

data EventEff a = ReleasePointerCapture Event a
derive instance functorEvEff :: Functor EventEff
type EVENTEFF = FProxy EventEff

releasePointerCapture :: ∀r. Event -> Run (eventEff :: EVENTEFF | r) Unit
releasePointerCapture ev = lift (SProxy :: SProxy "eventEff") (ReleasePointerCapture ev unit)

type EFFS = (rng :: RNG, delay :: DELAY, location :: LOCATION, pointer :: POINTER, event :: EVENT, eventEff :: EVENTEFF)

foreign import getLoc :: Effect Location
foreign import setTimeout :: Int -> Effect Unit -> Effect Unit
foreign import genNumber :: Effect Number

foreign import relativePointerPositionAux :: Maybe Position -> (Position -> Maybe Position) 
                                            -> Event -> Effect (Maybe Position)
relativePointerPosition :: Event -> Effect (Maybe Position)
relativePointerPosition = relativePointerPositionAux Nothing Just

foreign import releasePointerCaptureAux :: Event -> Effect Unit

interpretEffects :: InterpretEffs EFFS
interpretEffects = match {
    delay: interpretDelay,
    rng: interpretRng,
    location: \(GetLocation cont) -> getLoc >>= cont,
    pointer: \(GetPointer ev cont) -> relativePointerPosition ev >>= cont,
    event: interpretEvent,
    eventEff: case _ of
            ReleasePointerCapture ev cont -> releasePointerCaptureAux ev *> cont
}