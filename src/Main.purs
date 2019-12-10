module Main where

import MyPrelude hiding (view)
import Data.String (drop, indexOf) as S
import Data.String.Pattern (Pattern (..))
import Data.Map as Map
import Unsafe.Coerce (unsafeCoerce)
import Effect (Effect)
import Run as Run
import Pha (VDom, emptyNode, (<&&>), key, class_)
import Pha.Subs as Subs
import Pha.App (app, Document, attachTo)
import Pha.Update (Update, getState, setState)
import Pha.Events.Decoder (always)
import Pha.Lens (updateOver)
import Pha.Elements (div, a)
import Pha.Attributes (href)
import Pha.Svg (svg, use, width, height)
import Game.Generic (GenericGame)
import Game.Effs (EFFS, getLocation, interpretLocation, interpretDelay, interpretRng)
import Game.Baseball as Baseball
import Game.Chocolat as Chocolat
import Game.Dessin as Dessin
import Game.Frog as Frog
import Game.Jetons as Jetons
import Game.Labete as Labete
import Game.Nim as Nim
import Game.Noirblanc as Noirblanc
import Game.Paths  as Paths
import Game.Queens as Queens
import Game.Roue as Roue
import Game.Sansmot as Sansmot
import Game.Solitaire as Solitaire
import Game.Tiling as Tiling
import Game.Tricolor as Tricolor
import Game.Valise as Valise

infix 2 updateOver as .~>

extractLocation ∷ String → String → String
extractLocation url defaultValue =
    S.indexOf (Pattern "#") url # maybe defaultValue \i → S.drop (i + 1) url 

type RootState = 
    {   baseball ∷ Baseball.State
    ,   chocolat ∷ Chocolat.State
    ,   dessin ∷ Dessin.State
    ,   frog ∷ Frog.State
    ,   jetons ∷ Jetons.State
    ,   labete ∷ Labete.State
    ,   nim ∷ Nim.State
    ,   noirblanc ∷ Noirblanc.State
    ,   paths ∷ Paths.State
    ,   queens ∷ Queens.State
    ,   roue ∷ Roue.State
    ,   sansmot ∷ Sansmot.State
    ,   solitaire ∷ Solitaire.State
    ,   tiling ∷ Tiling.State
    ,   tricolor ∷ Tricolor.State
    ,   valise ∷ Valise.State
    ,   location ∷ String
    }

state ∷ RootState
state = 
    {   baseball: Baseball.istate
    ,   chocolat: Chocolat.istate
    ,   dessin: Dessin.istate
    ,   frog: Frog.istate
    ,   jetons: Jetons.istate
    ,   labete: Labete.istate
    ,   nim: Nim.istate
    ,   noirblanc: Noirblanc.istate
    ,   paths: Paths.istate
    ,   queens: Queens.istate
    ,   roue: Roue.istate
    ,   sansmot: Sansmot.istate
    ,   solitaire: Solitaire.istate
    ,   tiling: Tiling.istate
    ,   tricolor: Tricolor.istate
    ,   valise: Valise.istate
    ,   location: ""
    }

data Msg =
      BaseballMsg Baseball.Msg
    | ChocolatMsg Chocolat.Msg
    | DessinMsg Dessin.Msg
    | FrogMsg Frog.Msg
    | JetonsMsg Jetons.Msg
    | LabeteMsg Labete.Msg
    | NimMsg Nim.Msg
    | NoirblancMsg Noirblanc.Msg
    | PathsMsg Paths.Msg
    | QueensMsg Queens.Msg
    | RoueMsg Roue.Msg
    | SansmotMsg Sansmot.Msg 
    | SolitaireMsg Solitaire.Msg
    | ValiseMsg Valise.Msg
    | TilingMsg Tiling.Msg
    | TricolorMsg Tricolor.Msg
    | OnKeyDown String
    | OnHashChange

type GameWrapperF st msg =
    {   core ∷ GenericGame st msg
    ,   map ∷ RootState → st
    ,   msgmap ∷ msg → Msg
    }

foreign import data GameWrapper ∷ Type

gameWrap ∷ ∀st msg. GenericGame st msg → (RootState → st) → (msg → Msg) → GameWrapper
gameWrap core map msgmap = unsafeCoerce {core, map, msgmap}
gameRun ∷ ∀r. (∀st msg. GameWrapperF st msg → r) → GameWrapper → r
gameRun = unsafeCoerce
   
games ∷ Map.Map String GameWrapper
games = Map.fromFoldable 
    [   "baseball"  ∧ gameWrap Baseball.game  _.baseball  BaseballMsg
    ,   "chocolat"  ∧ gameWrap Chocolat.game  _.chocolat  ChocolatMsg
    ,   "dessin"    ∧ gameWrap Dessin.game    _.dessin    DessinMsg
    ,   "frog"      ∧ gameWrap Frog.game      _.frog      FrogMsg
    ,   "jetons"    ∧ gameWrap Jetons.game    _.jetons    JetonsMsg
    ,   "labete"    ∧ gameWrap Labete.game    _.labete    LabeteMsg
    ,   "nim"       ∧ gameWrap Nim.game       _.nim       NimMsg
    ,   "noirblanc" ∧ gameWrap Noirblanc.game _.noirblanc NoirblancMsg
    ,   "paths"     ∧ gameWrap Paths.game     _.paths     PathsMsg
    ,   "queens"    ∧ gameWrap Queens.game    _.queens    QueensMsg
    ,   "roue"      ∧ gameWrap Roue.game      _.roue      RoueMsg
    ,   "sansmot"   ∧ gameWrap Sansmot.game   _.sansmot   SansmotMsg
    ,   "solitaire" ∧ gameWrap Solitaire.game _.solitaire SolitaireMsg
    ,   "tiling"    ∧ gameWrap Tiling.game    _.tiling    TilingMsg
    ,   "tricolor"  ∧ gameWrap Tricolor.game  _.tricolor  TricolorMsg
    ,   "valise"    ∧ gameWrap Valise.game    _.valise    ValiseMsg
    ]

callByName ∷ ∀r. String → r → (∀st msg. GameWrapperF st msg → r) → r
callByName name default f = case games # Map.lookup name of
                                Nothing → default
                                Just game → game # gameRun f
 
hashChange ∷ Update RootState EFFS
hashChange = do
    loc ← getLocation
    let location = extractLocation loc.hash "valise"
    setState _{location = location}
    if location == "valise" then
        lens _.valise _{valise = _} .~> Valise.enterA
    else
        pure unit
    
 
update ∷ Msg → Update RootState EFFS
update (BaseballMsg msg)  = lens _.baseball _{baseball = _}   .~> Baseball.update msg
update (ChocolatMsg msg)  = lens _.chocolat _{chocolat = _}   .~> Chocolat.update msg
update (DessinMsg msg)    = lens _.dessin _{dessin = _}       .~> Dessin.update msg
update (FrogMsg msg)      = lens _.frog _{frog = _}           .~> Frog.update msg
update (JetonsMsg msg)    = lens _.jetons _{jetons = _}       .~> Jetons.update msg
update (LabeteMsg msg)    = lens _.labete _{labete = _}       .~> Labete.update msg
update (NimMsg msg)       = lens _.nim _{nim = _}             .~> Nim.update msg
update (NoirblancMsg msg) = lens _.noirblanc _{noirblanc = _} .~> Noirblanc.update msg
update (PathsMsg msg)     = lens _.paths _{paths = _}         .~> Paths.update msg
update (QueensMsg msg)    = lens _.queens _{queens = _}       .~> Queens.update msg
update (RoueMsg msg)      = lens _.roue _{roue = _}           .~> Roue.update msg
update (SansmotMsg msg)   = lens _.sansmot _{sansmot = _}     .~> Sansmot.update msg
update (SolitaireMsg msg) = lens _.solitaire _{solitaire = _} .~> Solitaire.update msg
update (TilingMsg msg)    = lens _.tiling _{tiling = _}       .~> Tiling.update msg
update (TricolorMsg msg)  = lens _.tricolor _{tricolor = _}   .~> Tricolor.update msg
update (ValiseMsg msg)    = lens _.valise _{valise = _}       .~> Valise.update msg
update (OnKeyDown k) = do 
        st ← getState
        callByName st.location (pure unit) \game →
            game.core.onKeydown k # maybe (pure unit) (update <<< game.msgmap)
update OnHashChange = hashChange

init ∷ Update RootState EFFS
init = do
    for_ (Map.values games) $
        gameRun \game → case game.core.init of
                            Nothing → pure unit
                            Just i → update $ game.msgmap i
    hashChange

view ∷ RootState → Document Msg
view st = {
    title: "Valise MaM",
    body:
        div
        [   key st.location
        ,   class_ "main-main-container"
        ,   class_ (if st.location == "valise" then "valise" else "game")
        ]
        [   st.location /= "valise" <&&> \_ →
                a
                [   class_ "main-minivalise-link"
                ,   href "#valise"
                ]
                [   svg [width "100%", height "100%"]
                    [   use "#valise" []]
                ]
        ,   viewGame st
        ]
}

viewGame ∷ RootState → VDom Msg
viewGame st = callByName st.location emptyNode 
                    \game → game.core.view (game.map st) <#> game.msgmap

main ∷ Effect Unit
main = app 
    {   init: state ∧ init
    ,   view
    ,   update
    ,   subscriptions: const
        [   Subs.onKeyDown (Just <<< OnKeyDown)
        ,   Subs.on "hashchange" (always OnHashChange)
        ]
    ,   interpreter: Run.match 
        {   delay: interpretDelay
        ,   rng: interpretRng
        ,   location: interpretLocation
        }
    } # attachTo "root"