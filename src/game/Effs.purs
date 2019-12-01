module Game.Effs (EFFS, LOCATION, Location, getLocation, GetLocation, interpretLocation, module E) where
import MyPrelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Run (FProxy, Run, SProxy(..), AFF)
import Run as Run
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
_location = SProxy :: SProxy "location"
getLocation :: ∀r. Run (location :: LOCATION | r) Location
getLocation = Run.lift _location (GetLocation identity)


type EFFS = (rng :: E.RNG, delay :: E.DELAY, location :: LOCATION)

foreign import getLoc :: Effect Location


interpretLocation ∷ ∀r. Run (aff ∷ AFF, location :: LOCATION | r) Unit → Run (aff ∷ AFF | r) Unit
interpretLocation = Run.run (Run.on _location handle Run.send) where
    handle ∷ GetLocation ~> Run (aff ∷ AFF | r)
    handle (GetLocation next) = do
        Run.liftAff $ liftEffect $ getLoc <#> next