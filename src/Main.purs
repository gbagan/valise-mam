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
import Game.Paths.Model (PathsState, pathsState)
import Game.Paths.View (view) as PathsView
import Game.Roue.Model (RoueState, roueState)
import Game.Roue.View (view) as RoueView
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
    paths :: PathsState,
    roue :: RoueState,
    solitaire :: SolitaireState,
    location :: String
}

_baseball :: Lens' RootState BaseballState
_baseball = lens (_.baseball) (_{baseball = _})

_frog :: Lens' RootState FrogState
_frog = lens (_.frog) (_{frog = _})

_jetons :: Lens' RootState JetonsState
_jetons = lens (_.jetons) (_{jetons = _})

_nim :: Lens' RootState NimState
_nim = lens (_.nim) (_{nim = _})

_noirblanc :: Lens' RootState NoirblancState
_noirblanc = lens (_.noirblanc) (_{noirblanc = _})

_paths :: Lens' RootState PathsState
_paths = lens (_.paths) (_{paths = _})

_roue :: Lens' RootState RoueState
_roue = lens (_.roue) (_{roue = _})

_solitaire :: Lens' RootState SolitaireState
_solitaire = lens (_.solitaire) (_{solitaire = _})

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
    "baseball" -> BaseballView.view _baseball state.baseball
    "frog" -> FrogView.view _frog state.frog
    "noirblanc" -> NoirblancView.view _noirblanc state.noirblanc
    "paths" -> PathsView.view _paths state.paths
    "roue" -> RoueView.view _roue state.roue
    "solitaire" -> SolitaireView.view _solitaire state.solitaire
    "jetons" -> JetonsView.view _jetons state.jetons
    _ -> NimView.view _nim state.nim

main :: Effect Unit
main = do
    locationHref <- getLocationHref
    let location = extractLocation locationHref "valise"
    baseballState' <- runRnd $ init baseballState
    nimState' <- runRnd $ init nimState
    frogState' <- runRnd $ init frogState
    jetonsState' <- runRnd $ init jetonsState
    noirblancState' <- runRnd $ init noirblancState
    pathState' <- runRnd $ init pathsState
    roueState' <- runRnd $ init roueState
    solitaireState' <- runRnd $ init solitaireState
    let state = {
        baseball: baseballState',
        nim: nimState',
        frog: frogState',
        jetons: jetonsState',
        noirblanc: noirblancState',
        paths: pathState',
        roue: roueState',
        solitaire: solitaireState',
        location: location
    }
    app {
        init: state,
        view,
        node: "root"
    }