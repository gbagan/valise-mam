module Main where
import Prelude
import Data.Maybe (maybe)
import Data.String (drop, indexOf)
import Data.String.Pattern (Pattern (..))
import Effect (Effect)
import Optic.Core (Lens', lens)
import Pha (VDom, app, Action(..))
import Lib.Game (init)
import Lib.Random (runRnd)
import Game.Baseball.Model (BaseballState, baseballState)
import Game.Baseball.View (view) as BaseballView
import Game.Nim.Model (NimState, nimState)
import Game.Nim.View (view) as NimView
import Game.Frog.Model (FrogState, frogState)
import Game.Frog.View (view) as FrogView

extractLocation :: String -> String -> String
extractLocation url defaultValue =
    let i = maybe 0 (add 1) $ indexOf (Pattern "#") url in drop i url


type RootState = {
    location :: String,
    baseball :: BaseballState,
    nim :: NimState,
    frog :: FrogState
}

baseballLens :: Lens' RootState BaseballState
baseballLens = lens (_.baseball) (_{baseball = _})

nimLens :: Lens' RootState NimState
nimLens = lens (_.nim) (_{nim = _})

frogLens :: Lens' RootState FrogState
frogLens = lens (_.frog) (_{frog = _})

foreign import getLocationHref :: Effect String

hashChange :: Action RootState
hashChange = Action \setState event state -> do
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
    _ -> NimView.view nimLens state.nim

main :: Effect Unit
main = do
    locationHref <- getLocationHref
    let location = extractLocation locationHref "valise"
    baseballState' <- runRnd $ init baseballState
    nimState' <- runRnd $ init nimState
    frogState' <- runRnd $ init frogState
    let state = {
        baseball: baseballState',
        nim: nimState',
        frog: frogState',
        location: location
    }
    app {
        init: state,
        view,
        node: "root"
    }