module Main where

import MyPrelude
import Data.String (drop, indexOf)
import Data.String.Pattern (Pattern (..))
import Effect (Effect)
import Pha (VDom, app, whenN)
import Pha.Action (Action, (🔍))
import Pha.Html (div', a, svguse, key, class', href)
import Pha.Event (key) as E
import Game (class CGame, init, view, onKeyDown) as G
import Game.Effs (EFFS, delay, getLoc, getLocation, getEvent, getState, setState, interpretEffects)
import Game.Baseball (State, state) as Baseball
import Game.Chocolat (State, state) as Chocolat
import Game.Dessin (State, state) as Dessin
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
import Game.Valise.Model (enterA, leaveA) as ValiseM


extractLocation :: String -> String -> String
extractLocation url defaultValue =
    indexOf (Pattern "#") url # maybe defaultValue \i -> drop (i + 1) url 

type RootState = {
    baseball :: Baseball.State, 
    chocolat :: Chocolat.State,
    dessin :: Dessin.State,
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

_dessin :: Lens' RootState Dessin.State
_dessin = lens (_.dessin) (_{dessin = _})

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

hashChange :: Action RootState EFFS
hashChange = do
    loc <- getLocation
    let location = extractLocation loc.hash "valise"
    if location == "valise" || location == "" then do
        setState (_{location = location, anim = true})
        delay 100
        setState (_{location = location, anim = false})
        _valise <<< Valise.is 🔍 ValiseM.enterA
    else do
        _valise <<< Valise.is 🔍 ValiseM.leaveA
        setState (_{location = location, anim = true})
        delay 100
        setState (_{location = location, anim = false})
    

init2 :: Action RootState EFFS
init2 = hashChange

sliceFn :: ∀a. RootState -> (∀b. G.CGame b => (Lens' RootState b) -> a)  -> a
sliceFn state fn = case state.location of
    "baseball" -> fn _baseball
    "chocolat" -> fn _chocolat
    "dessin" -> fn _dessin
    "frog" -> fn _frog
    "jetons" -> fn _jetons
    "labete" -> fn _labete
    "nim" -> fn _nim
    "noirblanc" -> fn _noirblanc
    "paths" -> fn _paths
    "roue" -> fn _roue
    "queens" -> fn _queens
    "solitaire" -> fn _solitaire
    "tiling" -> fn _tiling
    _ -> fn _valise

onKeyDown :: Action RootState EFFS
onKeyDown = do
    ev <- getEvent
    state <- getState
    case E.key ev of
        Nothing  -> pure unit
        Just k -> sliceFn state \lens -> lens 🔍 G.onKeyDown k

viewG :: RootState -> VDom RootState EFFS
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

viewGame :: RootState -> VDom RootState EFFS
viewGame st = sliceFn st \lens -> G.view lens (st ^. lens)

main :: Effect Unit
main = do
    loc <- getLoc
    let location = extractLocation loc.hash "valise"
    baseballState <- G.init Baseball.state
    chocolatState <- G.init Chocolat.state
    dessinState <- G.init Dessin.state
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
        dessin: dessinState,
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
        anim: true  --- empèche l'animation à l'ouverture de la page
    }
    app {
        state,
        view: viewG,
        node: "root",
        events: [
            Tuple "keydown" onKeyDown,
            Tuple "hashchange" hashChange
        ],
        effects: interpretEffects,
        init: init2
    }