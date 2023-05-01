module Main (main) where

import MamPrelude hiding (view)

import Data.Map as Map
import Data.String as String
import Effect (Effect)
import Effect.Ref as Ref
import Random.LCG (randomSeed)
import Control.Monad.Reader.Trans (runReaderT)
import Game.Baseball as Baseball
import Game.Bicolor as Bicolor
import Game.Chocolat as Chocolat
import Game.Dessin as Dessin
import Game.Eternal as Eternal
import Game.Frog as Frog
import Game.Generic (GenericGame)
import Game.Hanoi as Hanoi
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
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Update.Lens (updateOver)
import Lib.Update (UpdateMam, getHash)
import Pha.Subscriptions as Subs
import Unsafe.Coerce (unsafeCoerce)

infix 2 updateOver as .~>

type RootModel = 
    {   baseball ∷ Baseball.Model
    ,   bicolor ∷ Bicolor.Model
    ,   chocolat ∷ Chocolat.Model
    ,   dessin ∷ Dessin.Model
    ,   eternal ∷ Eternal.Model
    ,   frog ∷ Frog.Model
    ,   hanoi ∷ Hanoi.Model
    ,   jetons ∷ Jetons.Model
    ,   labete ∷ Labete.Model
    ,   nim ∷ Nim.Model
    ,   noirblanc ∷ Noirblanc.Model
    ,   paths ∷ Paths.Model
    ,   queens ∷ Queens.Model
    ,   roue ∷ Roue.Model
    ,   sansmot ∷ Sansmot.Model
    ,   solitaire ∷ Solitaire.Model
    ,   tiling ∷ Tiling.Model
    ,   tricolor ∷ Tricolor.Model
    ,   valise ∷ Valise.Model
    ,   location ∷ String
    }

appModel ∷ RootModel
appModel = 
    {   baseball: Baseball.imodel
    ,   chocolat: Chocolat.imodel
    ,   dessin: Dessin.imodel
    ,   eternal: Eternal.imodel
    ,   frog: Frog.imodel
    ,   hanoi: Hanoi.imodel
    ,   jetons: Jetons.imodel
    ,   labete: Labete.imodel
    ,   nim: Nim.imodel
    ,   noirblanc: Noirblanc.imodel
    ,   bicolor: Bicolor.imodel
    ,   paths: Paths.imodel
    ,   queens: Queens.imodel
    ,   roue: Roue.imodel
    ,   sansmot: Sansmot.imodel
    ,   solitaire: Solitaire.imodel
    ,   tiling: Tiling.imodel
    ,   tricolor: Tricolor.imodel
    ,   valise: Valise.imodel
    ,   location: ""
    }

data Msg =
      BaseballMsg Baseball.Msg
    | BicolorMsg Bicolor.Msg
    | ChocolatMsg Chocolat.Msg
    | DessinMsg Dessin.Msg
    | EternalMsg Eternal.Msg
    | FrogMsg Frog.Msg
    | HanoiMsg Hanoi.Msg
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

type GameWrapperF model msg =
    {   core ∷ GenericGame model msg
    ,   map ∷ RootModel → model
    ,   msgmap ∷ msg → Msg
    }

data GameWrapper

gameWrap ∷ ∀model msg. GenericGame model msg → (RootModel → model) → (msg → Msg) → GameWrapper
gameWrap core map msgmap = unsafeCoerce {core, map, msgmap}
gameRun ∷ ∀r. (∀model msg. GameWrapperF model msg → r) → GameWrapper → r
gameRun = unsafeCoerce
   
games ∷ Map String GameWrapper
games = Map.fromFoldable 
    [   "baseball"  ∧ gameWrap Baseball.game  _.baseball  BaseballMsg
    ,   "bicolor"   ∧ gameWrap Bicolor.game   _.bicolor   BicolorMsg
    ,   "chocolat"  ∧ gameWrap Chocolat.game  _.chocolat  ChocolatMsg
    ,   "dessin"    ∧ gameWrap Dessin.game    _.dessin    DessinMsg
    ,   "eternal"   ∧ gameWrap Eternal.game   _.eternal   EternalMsg
    ,   "frog"      ∧ gameWrap Frog.game      _.frog      FrogMsg
    ,   "hanoi"     ∧ gameWrap Hanoi.game     _.hanoi     HanoiMsg
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

callByName ∷ ∀r. String → r → (∀model msg. GameWrapperF model msg → r) → r
callByName name default f = case games # Map.lookup name of
                                Nothing → default
                                Just game → game # gameRun f 
 
update ∷ Msg → UpdateMam RootModel Unit
update (BaseballMsg msg)  = prop (Proxy ∷ Proxy "baseball")  .~> Baseball.update msg
update (BicolorMsg msg)   = prop (Proxy ∷ Proxy "bicolor")   .~> Bicolor.update msg
update (ChocolatMsg msg)  = prop (Proxy ∷ Proxy "chocolat")  .~> Chocolat.update msg
update (DessinMsg msg)    = prop (Proxy ∷ Proxy "dessin")    .~> Dessin.update msg
update (EternalMsg msg)   = prop (Proxy ∷ Proxy "eternal")   .~> Eternal.update msg
update (FrogMsg msg)      = prop (Proxy ∷ Proxy "frog")      .~> Frog.update msg
update (HanoiMsg msg)     = prop (Proxy ∷ Proxy "hanoi")     .~> Hanoi.update msg
update (JetonsMsg msg)    = prop (Proxy ∷ Proxy "jetons")    .~> Jetons.update msg
update (LabeteMsg msg)    = prop (Proxy ∷ Proxy "labete")    .~> Labete.update msg
update (NimMsg msg)       = prop (Proxy ∷ Proxy "nim")       .~> Nim.update msg
update (NoirblancMsg msg) = prop (Proxy ∷ Proxy "noirblanc") .~> Noirblanc.update msg
update (PathsMsg msg)     = prop (Proxy ∷ Proxy "paths")     .~> Paths.update msg
update (QueensMsg msg)    = prop (Proxy ∷ Proxy "queens")    .~> Queens.update msg
update (RoueMsg msg)      = prop (Proxy ∷ Proxy "roue")      .~> Roue.update msg
update (SansmotMsg msg)   = prop (Proxy ∷ Proxy "sansmot")   .~> Sansmot.update msg
update (SolitaireMsg msg) = prop (Proxy ∷ Proxy "solitaire") .~> Solitaire.update msg
update (TilingMsg msg)    = prop (Proxy ∷ Proxy "tiling")    .~> Tiling.update msg
update (TricolorMsg msg)  = prop (Proxy ∷ Proxy "tricolor")  .~> Tricolor.update msg
update (ValiseMsg msg)    = prop (Proxy ∷ Proxy "valise")    .~> Valise.update msg
update Init = init
update (KeyDown k) = do
        model' ← get
        callByName model'.location (pure unit) \game →
            for_ (game.core.onKeydown k) \msg →
                update (game.msgmap msg)
update HashChanged = do
    hash ← getHash
    let location = String.drop 1 hash
    modify_ _{location = location}
    if location == "" then
        prop (Proxy ∷ Proxy "valise") .~> Valise.enterA
    else
        pure unit

init ∷ UpdateMam RootModel Unit
init = do
    for_ (Map.values games) $
        gameRun \game → case game.core.init of
                            Nothing → pure unit
                            Just init' → update $ game.msgmap init'
    update HashChanged
    

view ∷ RootModel → Html Msg
view model =
  H.div []
    [ H.div
      [ H.class_ "main-main-container"
      , H.class_ (if model.location == "" then "valise" else "game")
      ]
      [ H.when (model.location ≠ "") \_ →
          H.a
            [ H.class_ "main-minivalise-link"
            , P.href "#"
            ]
            [ H.svg [P.width "100%", P.height "100%"]
                [ H.use [P.href "#valise"]]
            ]
      , viewGame model
      ]
    ]

viewGame ∷ RootModel → Html Msg
viewGame model = callByName model.location H.empty
                    \game → game.msgmap <$> game.core.view (game.map model) 

main ∷ Effect Unit
main = do
    newSeed <- randomSeed
    genModel <- Ref.new {newSeed, size: 0}
    app {   init: {state: appModel, action: Just Init}
        ,   view
        ,   update
        ,   eval: flip runReaderT {genModel}
        ,   subscriptions: [Subs.onKeyDown (Just ∘ KeyDown), Subs.onHashChange $ const (Just HashChanged)]
        ,   selector: "#root"
        }