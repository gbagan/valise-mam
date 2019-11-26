module Game.Effs (EFFS, LOCATION, Location, getLocation, GetLocation, interpretEffects, module E) where
import MyPrelude
import Effect (Effect)
import Run (FProxy, Run, SProxy(..), lift, match)
import Pha (InterpretEffs, Event)
import Pha.Effects.Delay (DELAY, interpretDelay) as E
import Pha.Effects.Random (RNG, interpretRng) as E

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


type EFFS = (rng :: E.RNG, delay :: E.DELAY, location :: LOCATION)

foreign import getLoc :: Effect Location

interpretEffects :: InterpretEffs EFFS
interpretEffects = match {
    delay: E.interpretDelay,
    rng: E.interpretRng,
    location: \(GetLocation cont) -> getLoc >>= cont
}