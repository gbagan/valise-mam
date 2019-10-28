module Main where
import Prelude
import Data.Maybe (maybe)
import Data.Lens (Lens', lens)
import Data.String (drop, indexOf)
import Data.String.Pattern (Pattern (..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Pha.Class (VDom, Action(..)) 
import Pha (app)
import Game.Core (init)
import Lib.Random (runRnd)
import Game.Baseball.Model (BaseballState, baseballState)
import Game.Baseball.View (view) as BaseballView
import Game.Nim.Model (NimState, nimState)
import Game.Nim.View (view) as NimView
import Game.Frog.Model (FrogState, frogState)
import Game.Frog.View (view) as FrogView
import Game.Jetons.Model (JetonsState, jetonsState)
import Game.Jetons.View (view) as JetonsView
import Game.Noirblanc.Model (NoirblancState, noirblancState)
import Game.Noirblanc.View (view) as NoirblancView
import Game.Solitaire.Model (SolitaireState, solitaireState)
import Game.Solitaire.View (view) as SolitaireView

extractLocation :: String -> String -> String
extractLocation url defaultValue =
    let i = maybe 0 (add 1) $ indexOf (Pattern "#") url in drop i url


type RootState = {
    baseball :: BaseballState,
    frog :: FrogState,
    jetons :: JetonsState,
    nim :: NimState,
    noirblanc :: NoirblancState,
    solitaire :: SolitaireState,
    location :: String
}

baseballLens :: Lens' RootState BaseballState
baseballLens = lens (_.baseball) (_{baseball = _})

nimLens :: Lens' RootState NimState
nimLens = lens (_.nim) (_{nim = _})

frogLens :: Lens' RootState FrogState
frogLens = lens (_.frog) (_{frog = _})

jetonsLens :: Lens' RootState JetonsState
jetonsLens = lens (_.jetons) (_{jetons = _})

noirblancLens :: Lens' RootState NoirblancState
noirblancLens = lens (_.noirblanc) (_{noirblanc = _})

solitaireLens :: Lens' RootState SolitaireState
solitaireLens = lens (_.solitaire) (_{solitaire = _})

foreign import getLocationHref :: Effect String

hashChange :: Action RootState
hashChange = Action \setState event state -> liftEffect $ do
    locationHref <- getLocationHref
    let location = extractLocation locationHref "valise"
    setState $ state{location = location}
    --    } |> combine(
    --        enter(location2),
    --        state.location !== location && asyncToggle('anim', 50)
    --    );
    -- };

view :: RootState -> VDom RootState
view state = case state.location of
    "baseball" -> BaseballView.view baseballLens state.baseball
    "frog" -> FrogView.view frogLens state.frog
    "noirblanc" -> NoirblancView.view noirblancLens state.noirblanc
    "solitaire" -> SolitaireView.view solitaireLens state.solitaire
    "jetons" -> JetonsView.view jetonsLens state.jetons
    _ -> NimView.view nimLens state.nim

main :: Effect Unit
main = do
    locationHref <- getLocationHref
    let location = extractLocation locationHref "valise"
    baseballState' <- runRnd $ init baseballState
    nimState' <- runRnd $ init nimState
    frogState' <- runRnd $ init frogState
    jetonsState' <- runRnd $ init jetonsState
    noirblancState' <- runRnd $ init noirblancState
    solitaireState' <- runRnd $ init solitaireState
    let state = {
        baseball: baseballState',
        nim: nimState',
        frog: frogState',
        jetons: jetonsState',
        noirblanc: noirblancState',
        solitaire: solitaireState',
        location: location
    }
    app {
        init: state,
        view,
        node: "root"
    }