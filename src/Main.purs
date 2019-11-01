module Main where
import Prelude
import Data.Lens (Lens', lens)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (drop, indexOf)
import Data.String.Pattern (Pattern (..))
import Effect (Effect)
import Pha (VDom, app)
import Pha.Action (Action, (🎲), asyncAction, withPayload, withPayload')
import Pha.Event (key) as E
import Lib.Random (runRnd)
import Game.Core (init)
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
import Game.Queens.Model (QueensState, queensState)
import Game.Queens.View (view) as QueensView
import Game.Roue.Model (RoueState, roueState)
import Game.Roue.View (view) as RoueView
import Game.Solitaire.Model (SolitaireState, solitaireState)
import Game.Solitaire.View (view) as SolitaireView
import Game.Tiling.Model (TilingState, tilingState)
import Game.Tiling.View (view) as TilingView
import Game.Valise.Model (ValiseState, valiseState)
import Game.Valise.View (view) as ValiseView

import Game.Frog.Model (onKeyDown) as Frog
import Game.Noirblanc.Model (onKeyDown) as Noirblanc
import Game.Tiling.Model (onKeyDown) as Tiling
import Game.Valise.Model (enterA) as Valise

extractLocation :: String -> String -> String
extractLocation url defaultValue =
    indexOf (Pattern "#") url # maybe defaultValue \i -> drop (i + 1) url 


type RootState = {
    baseball :: BaseballState,
    frog :: FrogState,
    jetons :: JetonsState,
    nim :: NimState,
    noirblanc :: NoirblancState,
    paths :: PathsState,
    queens :: QueensState,
    roue :: RoueState,
    solitaire :: SolitaireState,
    tiling :: TilingState,
    valise :: ValiseState,
    location :: String
}

-- class Game pos ext mov <= GameLens pos ext mov | ext -> pos mov where
--    gamelens :: Lens' RootState (State pos ext)
-- instance baseballLens :: GameLens 

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

_queens :: Lens' RootState QueensState
_queens = lens (_.queens) (_{queens = _})

_roue :: Lens' RootState RoueState
_roue = lens (_.roue) (_{roue = _})

_solitaire :: Lens' RootState SolitaireState
_solitaire = lens (_.solitaire) (_{solitaire = _})

_tiling :: Lens' RootState TilingState
_tiling = lens (_.tiling) (_{tiling = _})

_valise :: Lens' RootState ValiseState
_valise = lens (_.valise) (_{valise = _})

foreign import getLocationHref :: Effect String

hashChange :: String -> Action RootState
hashChange hash = asyncAction \{updateState, dispatch} -> do
    let location = extractLocation hash "valise"
    _ <- updateState (_{location = location})
    if location == "valise" || location == ""  then do
        _ <- dispatch (_valise 🎲 Valise.enterA)
        pure unit
    else
        pure unit
    --    } |> combine(
    --        enter(location2),
    --        state.location !== location && asyncToggle('anim', 50)
    --    );
    -- };

init2 :: Action RootState
init2 = hashChange `withPayload'` \e -> getLocationHref

onKeyDown :: (Maybe String) -> Action RootState
onKeyDown maybek = asyncAction \{dispatch, getState} ->
    case maybek of
        Nothing -> pure unit
        Just k -> do
            state <- getState
            case state.location of
                "tiling" -> dispatch (_tiling 🎲 Tiling.onKeyDown k)
                "noirblanc" -> dispatch (_noirblanc 🎲 Noirblanc.onKeyDown k)
                "frog" -> dispatch (_frog 🎲 Frog.onKeyDown k)
                _ -> pure unit

view :: RootState -> VDom RootState
view state = case state.location of
    "baseball" -> BaseballView.view _baseball state.baseball
    "frog" -> FrogView.view _frog state.frog
    "jetons" -> JetonsView.view _jetons state.jetons
    "noirblanc" -> NoirblancView.view _noirblanc state.noirblanc
    "paths" -> PathsView.view _paths state.paths
    "roue" -> RoueView.view _roue state.roue
    "queens" -> QueensView.view _queens state.queens
    "solitaire" -> SolitaireView.view _solitaire state.solitaire
    "tiling" -> TilingView.view _tiling state.tiling
    "nim" -> NimView.view _nim state.nim
    _ -> ValiseView.view _valise state.valise

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
    queensState' <- runRnd $ init queensState
    roueState' <- runRnd $ init roueState
    solitaireState' <- runRnd $ init solitaireState
    tilingState' <- runRnd $ init tilingState
    let state = {
        baseball: baseballState',
        nim: nimState',
        frog: frogState',
        jetons: jetonsState',
        noirblanc: noirblancState',
        paths: pathState',
        queens: queensState',
        roue: roueState',
        solitaire: solitaireState',
        tiling: tilingState',
        valise: valiseState,
        location: location
    }
    app {
        state,
        view,
        node: "root",
        events: [Tuple "keydown" (onKeyDown `withPayload` E.key),
                Tuple "hashchange" (hashChange `withPayload'` \e -> getLocationHref)
                ],
        init: init2
    }