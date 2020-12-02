module Main (main) where

import MyPrelude hiding (view)

import Data.Map as Map
import Data.String as String
import Effect (Effect)
import Game.Baseball as Baseball
import Game.Chocolat as Chocolat
import Game.Dessin as Dessin
import Game.Eternal as Eternal
import Game.Frog as Frog
import Game.Generic (GenericGame)
import Game.Jetons as Jetons
import Game.Labete as Labete
import Game.Nim as Nim
import Game.Noirblanc as Noirblanc
import Game.Paths as Paths
import Game.Queens as Queens
import Game.Roue as Roue
import Game.Sansmot as Sansmot
import Game.Solitaire as Solitaire
import Game.Tiling as Tiling
import Game.Tricolor as Tricolor
import Game.Valise as Valise
import Pha.App (app)
import Pha (VDom)
import Pha as H
import Pha.Elements as HH
import Pha.Attributes as P
import Lib.Update (Update, get, modify, getHash, updateOver, interpret)
import Pha.Subs as Subs
import Unsafe.Coerce (unsafeCoerce)

infix 2 updateOver as .~>

type RootState = 
    {   baseball ∷ Baseball.State
    ,   chocolat ∷ Chocolat.State
    ,   dessin ∷ Dessin.State
    ,   eternal ∷ Eternal.State
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
    ,   eternal: Eternal.istate
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
    | EternalMsg Eternal.Msg
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
    | KeyDown String
    | HashChanged
    | Init

type GameWrapperF st msg =
    {   core ∷ GenericGame st msg
    ,   map ∷ RootState → st
    ,   msgmap ∷ msg → Msg
    }

data GameWrapper

gameWrap ∷ ∀st msg. GenericGame st msg → (RootState → st) → (msg → Msg) → GameWrapper
gameWrap core map msgmap = unsafeCoerce {core, map, msgmap}
gameRun ∷ ∀r. (∀st msg. GameWrapperF st msg → r) → GameWrapper → r
gameRun = unsafeCoerce
   
games ∷ Map String GameWrapper
games = Map.fromFoldable 
    [   "baseball"  ∧ gameWrap Baseball.game  _.baseball  BaseballMsg
    ,   "chocolat"  ∧ gameWrap Chocolat.game  _.chocolat  ChocolatMsg
    ,   "dessin"    ∧ gameWrap Dessin.game    _.dessin    DessinMsg
    ,   "eternal"   ∧ gameWrap Eternal.game   _.eternal   EternalMsg
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
    ,   ""          ∧ gameWrap Valise.game    _.valise    ValiseMsg
    ]

callByName ∷ ∀r. String → r → (∀st msg. GameWrapperF st msg → r) → r
callByName name default f = case games # Map.lookup name of
                                Nothing → default
                                Just game → game # gameRun f 
 
update ∷ Msg → Update RootState
update (BaseballMsg msg)  = prop (SProxy ∷ SProxy "baseball")  .~> Baseball.update msg
update (ChocolatMsg msg)  = prop (SProxy ∷ SProxy "chocolat")  .~> Chocolat.update msg
update (DessinMsg msg)    = prop (SProxy ∷ SProxy "dessin")    .~> Dessin.update msg
update (EternalMsg msg)   = prop (SProxy ∷ SProxy "eternal")   .~> Eternal.update msg
update (FrogMsg msg)      = prop (SProxy ∷ SProxy "frog")      .~> Frog.update msg
update (JetonsMsg msg)    = prop (SProxy ∷ SProxy "jetons")    .~> Jetons.update msg
update (LabeteMsg msg)    = prop (SProxy ∷ SProxy "labete")    .~> Labete.update msg
update (NimMsg msg)       = prop (SProxy ∷ SProxy "nim")       .~> Nim.update msg
update (NoirblancMsg msg) = prop (SProxy ∷ SProxy "noirblanc") .~> Noirblanc.update msg
update (PathsMsg msg)     = prop (SProxy ∷ SProxy "paths")     .~> Paths.update msg
update (QueensMsg msg)    = prop (SProxy ∷ SProxy "queens")    .~> Queens.update msg
update (RoueMsg msg)      = prop (SProxy ∷ SProxy "roue")      .~> Roue.update msg
update (SansmotMsg msg)   = prop (SProxy ∷ SProxy "sansmot")   .~> Sansmot.update msg
update (SolitaireMsg msg) = prop (SProxy ∷ SProxy "solitaire") .~> Solitaire.update msg
update (TilingMsg msg)    = prop (SProxy ∷ SProxy "tiling")    .~> Tiling.update msg
update (TricolorMsg msg)  = prop (SProxy ∷ SProxy "tricolor")  .~> Tricolor.update msg
update (ValiseMsg msg)    = prop (SProxy ∷ SProxy "valise")    .~> Valise.update msg
update Init = init
update (KeyDown k) = do
        st ← get
        callByName st.location (pure unit) \game →
            case game.core.onKeydown k of
                Nothing → pure unit
                Just msg → update (game.msgmap msg)
update HashChanged = do
    hash ← getHash
    let location = String.drop 1 hash
    modify _{location = location}
    if location == "" then
        prop (SProxy ∷ SProxy "valise") .~> Valise.enterA
    else
        pure unit

init ∷ Update RootState
init = do
    for_ (Map.values games) $
        gameRun \game → case game.core.init of
                            Nothing → pure unit
                            Just init' → update $ game.msgmap init'
    update HashChanged
    

view ∷ RootState → VDom Msg
view st =
    HH.div
    [   H.key st.location
    ,   H.class_ "main-main-container"
    ,   H.class_ (if st.location == "" then "valise" else "game")
    ]
    [   H.when (st.location ≠ "") \_ →
        HH.a
        [   H.class_ "main-minivalise-link"
        ,   P.href "#"
        ]
        [   HH.svg [P.width "100%", P.height "100%"]
            [   HH.use [P.href "#valise"]]
        ]
    ,   viewGame st
    ]

viewGame ∷ RootState → H.VDom Msg
viewGame st = callByName st.location H.emptyNode 
                    \game → game.core.view (game.map st) <#> game.msgmap

main ∷ Effect Unit
main = app
    {   init: {state, action: Just $ Init}
    ,   view
    ,   update: \helpers msg → interpret helpers (update msg)
    ,   subscriptions: const [Subs.onKeyDown (Just ∘ KeyDown), Subs.onHashChange $ const (Just HashChanged)]
    ,   selector: "#root"
    }