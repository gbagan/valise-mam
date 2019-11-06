module Main where

{-
import MyPrelude
import Data.String (drop, indexOf)
import Data.String.Pattern (Pattern (..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, forkAff)
import Pha (VDom, app, whenN)
import Pha.Action (Action, (🔍), asyncAction, withPayload, withPayload')
import Pha.Html (div', a, svguse, key, class', href)
import Pha.Event (key) as E
import Game (class CGame, init, view, onKeyDown) as G

import Game.Baseball (State, state) as Baseball
import Game.Chocolat (State, state) as Chocolat
import Game.Frog (State, state) as Frog
import Game.Jetons (State, state) as Jetons
import Game.Labete (State, state) as Labete
import Game.Nim (State, state) as Nim
import Game.Noirblanc (State, state) as Noirblanc
import Game.Paths (State, state) as Paths
import Game.Queens (State, state) as Queens
import Game.Roue (State, state) as Roue
import Game.Solitaire (State, state) as Solitaire
import Game.Tiling (State, state) as Tiling
import Game.Valise (State, state, is) as Valise
import Game.Valise.Model (enterA) as ValiseM

extractLocation :: String -> String -> String
extractLocation url defaultValue =
    indexOf (Pattern "#") url # maybe defaultValue \i -> drop (i + 1) url 

type RootState = {
    baseball :: Baseball.State,
    chocolat :: Chocolat.State,
    frog :: Frog.State,
    jetons :: Jetons.State,
    labete :: Labete.State,
    nim :: Nim.State,
    noirblanc :: Noirblanc.State,
    paths :: Paths.State,
    queens :: Queens.State,
    roue :: Roue.State,
    solitaire :: Solitaire.State,
    tiling :: Tiling.State,
    valise :: Valise.State,
    location :: String,
    anim :: Boolean
}

_baseball :: Lens' RootState Baseball.State
_baseball = lens (_.baseball) (_{baseball = _})

_chocolat :: Lens' RootState Chocolat.State
_chocolat = lens (_.chocolat) (_{chocolat = _})

_frog :: Lens' RootState Frog.State
_frog = lens (_.frog) (_{frog = _})

_jetons :: Lens' RootState Jetons.State
_jetons = lens (_.jetons) (_{jetons = _})

_labete :: Lens' RootState Labete.State
_labete = lens (_.labete) (_{labete = _})

_nim :: Lens' RootState Nim.State
_nim = lens (_.nim) (_{nim = _})

_noirblanc :: Lens' RootState Noirblanc.State
_noirblanc = lens (_.noirblanc) (_{noirblanc = _})

_paths :: Lens' RootState Paths.State
_paths = lens (_.paths) (_{paths = _})

_queens :: Lens' RootState Queens.State
_queens = lens (_.queens) (_{queens = _})

_roue :: Lens' RootState Roue.State
_roue = lens (_.roue) (_{roue = _})

_solitaire :: Lens' RootState Solitaire.State
_solitaire = lens (_.solitaire) (_{solitaire = _})

_tiling :: Lens' RootState Tiling.State
_tiling = lens (_.tiling) (_{tiling = _})

_valise :: Lens' RootState Valise.State
_valise = lens (_.valise) (_{valise = _})


foreign import getLocationHref :: Effect String

hashChange :: String -> Action RootState
hashChange hash = asyncAction \{getState, updateState, dispatch} _ -> do
    let location = extractLocation hash "valise"
    _ <- updateState (_{location = location, anim = true})
    _ <- forkAff $ do
        delay $ Milliseconds 100.0
        updateState (_{location = location, anim = false})
    if location == "valise" || location == "" then do
        dispatch ((_valise <<< Valise.is) 🔍 ValiseM.enterA)
    else
        getState

init2 :: Action RootState
init2 = hashChange `withPayload'` \e -> getLocationHref

sliceFn :: forall a. RootState -> (forall b. G.CGame b => (Lens' RootState b) -> a)  -> a
sliceFn state fn = case state.location of
    "baseball" -> fn _baseball
    "chocolat" -> fn _chocolat
    "frog" -> fn _frog
    "jetons" -> fn _jetons
    "labete" -> fn _labete
    "noirblanc" -> fn _noirblanc
    "paths" -> fn _paths
    "roue" -> fn _roue
    "queens" -> fn _queens
    "solitaire" -> fn _solitaire
    "tiling" -> fn _tiling
    "nim" -> fn _nim
    _ -> fn _valise

onKeyDown :: (Maybe String) -> Action RootState
onKeyDown k = asyncAction \{dispatch} state ->
    k # maybe (pure state) \k' -> dispatch (sliceFn state \lens -> lens 🔍 G.onKeyDown k')


viewG :: RootState -> VDom RootState
viewG state = div' [
    key state.location,
    class' "main-main-container" true,
    class' "valise" $ state.location == "valise",
    class' "appear" state.anim
] [
    whenN (state.location /= "valise") \_ ->
        a [
            class' "main-minivalise-link" true,
            href "#valise"
        ] [svguse "#valise" []],
    viewGame state
]

viewGame :: RootState -> VDom RootState
viewGame st = sliceFn st \lens -> G.view lens (st ^. lens)

main :: Effect Unit
main = do
    locationHref <- getLocationHref
    let location = extractLocation locationHref "valise"
    baseballState <- G.init Baseball.state
    chocolatState <- G.init Chocolat.state
    frogState <- G.init Frog.state
    jetonsState <- G.init Jetons.state
    labeteState <- G.init Labete.state
    nimState <- G.init Nim.state
    noirblancState <- G.init Noirblanc.state
    pathState <- G.init Paths.state
    queensState <- G.init Queens.state
    roueState <- G.init Roue.state
    solitaireState <- G.init Solitaire.state
    tilingState <- G.init Tiling.state
    valiseState <- G.init Valise.state

    let state = {
        baseball: baseballState,
        chocolat: chocolatState,
        frog: frogState,
        jetons: jetonsState,
        labete: labeteState,
        nim: nimState,
        noirblanc: noirblancState,
        paths: pathState,
        queens: queensState,
        roue: roueState,
        solitaire: solitaireState,
        tiling: tilingState,
        valise: valiseState,
        location: location,
        anim: false
    }
    app {
        state,
        view: viewG,
        node: "root",
        events: [
                Tuple "keydown" (onKeyDown `withPayload` E.key),
                Tuple "hashchange" (hashChange `withPayload'` \e -> getLocationHref)
        ],
        init: init2
    }