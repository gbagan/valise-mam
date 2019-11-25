module Main where

import MyPrelude hiding (view)
import Data.String (drop, indexOf) as S
import Data.String.Pattern (Pattern (..))
import Effect (Effect)
import Pha (VDom, Event, app, ifN, key, class')
import Pha.Action (Action, getState, setState)
import Pha.Effects.Delay (delay)
import Pha.Lens (actionOver)
import Pha.Elements (div, a)
import Pha.Attributes (href)
import Pha.Svg (svg, use, width, height)
import Pha.Event (key) as E
import Game.Effs (EFFS, getLocation, interpretEffects)
import Game.Baseball as Baseball
--import Game.Chocolat (State, state) as Chocolat
--import Game.Dessin (State, state) as Dessin
--import Game.Frog (State, state) as Frog
--import Game.Jetons (State, state) as Jetons
--import Game.Labete (State, state) as Labete
import Game.Nim as Nim
--import Game.Noirblanc (State, state) as Noirblanc
--import Game.Paths (State, state) as Paths
--import Game.Queens (State, state) as Queens
--import Game.Roue (State, state) as Roue
--import Game.Sansmot (State, state) as Sansmot
--import Game.Solitaire (State, state) as Solitaire
--import Game.Tiling (State, state) as Tiling
--import Game.Tricolor (State, state) as Tricolor
--import Game.Valise (State, state, _iso) as Valise
--import Game.Valise.Model (enterA, leaveA) as ValiseM


extractLocation :: String -> String -> String
extractLocation url defaultValue =
    S.indexOf (Pattern "#") url # maybe defaultValue \i -> S.drop (i + 1) url 

type RootState = {
    baseball :: Baseball.State,
--    chocolat :: Chocolat.State,
--    dessin :: Dessin.State,
--    frog :: Frog.State,
--    jetons :: Jetons.State,
--    labete :: Labete.State,
    nim :: Nim.State,
--    noirblanc :: Noirblanc.State,
--    paths :: Paths.State,
--    queens :: Queens.State,
--    roue :: Roue.State,
--    sansmot :: Sansmot.State,
--    solitaire :: Solitaire.State,
 --   tiling :: Tiling.State,
 --   tricolor :: Tricolor.State,
 --   valise :: Valise.State,-}
    location :: String,
    anim :: Boolean
}

data Msg =
      BaseballMsg Baseball.Msg
    | NimMsg Nim.Msg

_baseball :: Lens' RootState Baseball.State
_baseball = lens _.baseball _{baseball = _}

--_chocolat :: Lens' RootState Chocolat.State
--_chocolat = lens _.chocolat _{chocolat = _}

--_dessin :: Lens' RootState Dessin.State
--_dessin = lens _.dessin _{dessin = _}

--_frog :: Lens' RootState Frog.State
--_frog = lens _.frog _{frog = _}

--_jetons :: Lens' RootState Jetons.State
--_jetons = lens _.jetons _{jetons = _}

--_labete :: Lens' RootState Labete.State
--_labete = lens _.labete _{labete = _}

_nim :: Lens' RootState Nim.State
_nim = lens _.nim _{nim = _}

--_noirblanc :: Lens' RootState Noirblanc.State
--_noirblanc = lens _.noirblanc _{noirblanc = _}

--_paths :: Lens' RootState Paths.State
--_paths = lens _.paths _{paths = _}

--_queens :: Lens' RootState Queens.State
--_queens = lens _.queens _{queens = _}

--_roue :: Lens' RootState Roue.State
--_roue = lens _.roue _{roue = _}

--_sansmot :: Lens' RootState Sansmot.State
--_sansmot = lens _.sansmot _{sansmot = _}

--_solitaire :: Lens' RootState Solitaire.State
--_solitaire = lens _.solitaire _{solitaire = _}

--_tiling :: Lens' RootState Tiling.State
--_tiling = lens _.tiling _{tiling = _}

--_tricolor :: Lens' RootState Tricolor.State
--_tricolor = lens _.tricolor _{tricolor = _}

--_valise :: Lens' RootState Valise.State
--_valise = lens _.valise _{valise = _}

hashChange :: forall a. a -> Action RootState EFFS
hashChange _ = do
    loc <- getLocation
    let location = extractLocation loc.hash "valise"
    if location == "valise" || location == "" then do
        setState _{location = location, anim = true}
        delay 100
        setState _{anim = false}
        --actionOver (_valise ∘ Valise._iso) ValiseM.enterA
    else do
        --actionOver (_valise ∘ Valise._iso) ValiseM.leaveA
        setState _{location = location, anim = true}
        delay 100
        setState _{anim = false}
    

update :: Msg -> Action RootState EFFS
update (BaseballMsg msg) = actionOver _baseball (Baseball.update msg)
update (NimMsg msg)      = actionOver _nim (Nim.update msg)

init :: Action RootState EFFS
init = do
    actionOver _baseball Baseball.init
    --actionOver _chocolat G.init
    --actionOver _dessin G.init
    --actionOver _frog G.init
    --actionOver _jetons G.init
    --actionOver _labete G.init
    actionOver _nim Nim.init
    --actionOver _noirblanc G.init
    --actionOver _paths G.init
    --actionOver _queens G.init
    --actionOver _roue G.init
    --actionOver _sansmot G.init
    --actionOver _solitaire G.init
    --actionOver _tiling G.init
    --actionOver _tricolor G.init
    --actionOver _valise G.init
    hashChange unit

{-
sliceFn :: ∀a. RootState -> (∀b. G.CGame b => (Lens' RootState b) -> a)  -> a
sliceFn st fn = case st.location of
    _ -> fn _baseball
 --   "chocolat" -> fn _chocolat
 --   "dessin" -> fn _dessin
 --   "frog" -> fn _frog
 --   "jetons" -> fn _jetons
 --   "labete" -> fn _labete
 --   "nim" -> fn _nim
 --   "noirblanc" -> fn _noirblanc
 --   "paths" -> fn _paths
 --   "roue" -> fn _roue
 --   "queens" -> fn _queens
 --   "sansmot" -> fn _sansmot
 --   "solitaire" -> fn _solitaire
 --   "tiling" -> fn _tiling
 --   "tricolor" -> fn _tricolor
 --   _ -> fn _valise
-}

onKeyDown :: Event -> Action RootState EFFS
onKeyDown ev = do
    st <- getState
    case E.key ev of
        _  -> pure unit
        -- Just k -> sliceFn st \lens -> actionOver lens (G.onKeyDown k)

view :: RootState -> VDom Msg
view st = div [
    key st.location,
    class' "main-main-container" true,
    class' "valise" $ st.location == "valise",
    class' "appear" st.anim
] [
    ifN (st.location /= "valise") \_ ->
        a [
            class' "main-minivalise-link" true,
            href "#valise"
        ] [svg [width "100%", height "100%"] [use "#valise" []]],
    viewGame st
]

viewGame :: RootState -> VDom Msg
viewGame st = case st.location of
    "baseball" -> Baseball.view st.baseball <#> BaseballMsg
    _ -> Nim.view st.nim <#> NimMsg
 --   "dessin" -> fn _dessin

state :: RootState
state = {
    baseball: Baseball.istate,
 --   chocolat: Chocolat.state,
 --   dessin: Dessin.state,
--    frog: Frog.state,
--    jetons: Jetons.state,
--    labete: Labete.state,
     nim: Nim.istate,
--    noirblanc: Noirblanc.state,
--    paths: Paths.state,
--    queens: Queens.state,
--    roue: Roue.state,
--    sansmot: Sansmot.state,
 --   solitaire: Solitaire.state,
 --   tiling: Tiling.state,
 --   tricolor: Tricolor.state,
 --   valise: Valise.state,
    location: "",
    anim: true  --- empèche l'animation à l'ouverture de la page
}

main :: Effect Unit
main = app {
    state,
    view,
    update,
    init,
    node: "root",
    events: [
        "keydown" ∧ onKeyDown,
        "hashchange" ∧ hashChange
    ],
    interpret: interpretEffects
}