module Main (main) where

import MamPrelude hiding (view)

import Control.Monad.Reader.Trans (runReaderT)
import Data.Map as Map
import Data.String as String
import Effect (Effect)
import Effect.Ref as Ref
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
import Lib.Update (UpdateMam, getHash)
import Pha.App (app)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Svg as S
import Pha.Subscriptions as Subs
import Pha.Update (hoist, mapMessage, mapModel)
import Random.LCG (randomSeed)
import Unsafe.Coerce (unsafeCoerce)

infix 2 mapModel as .~>

type Model =
  { baseball ∷ Baseball.Model
  , bicolor ∷ Bicolor.Model
  , chocolat ∷ Chocolat.Model
  , dessin ∷ Dessin.Model
  , eternal ∷ Eternal.Model
  , frog ∷ Frog.Model
  , hanoi ∷ Hanoi.Model
  , jetons ∷ Jetons.Model
  , labete ∷ Labete.Model
  , nim ∷ Nim.Model
  , noirblanc ∷ Noirblanc.Model
  , paths ∷ Paths.Model
  , queens ∷ Queens.Model
  , roue ∷ Roue.Model
  , sansmot ∷ Sansmot.Model
  , solitaire ∷ Solitaire.Model
  , tiling ∷ Tiling.Model
  , tricolor ∷ Tricolor.Model
  , valise ∷ Valise.Model
  , location ∷ String
  }

appModel ∷ Model
appModel =
  { baseball: Baseball.imodel
  , chocolat: Chocolat.imodel
  , dessin: Dessin.imodel
  , eternal: Eternal.imodel
  , frog: Frog.imodel
  , hanoi: Hanoi.imodel
  , jetons: Jetons.imodel
  , labete: Labete.imodel
  , nim: Nim.imodel
  , noirblanc: Noirblanc.imodel
  , bicolor: Bicolor.imodel
  , paths: Paths.imodel
  , queens: Queens.imodel
  , roue: Roue.imodel
  , sansmot: Sansmot.imodel
  , solitaire: Solitaire.imodel
  , tiling: Tiling.imodel
  , tricolor: Tricolor.imodel
  , valise: Valise.imodel
  , location: ""
  }

data Msg
  = BaseballMsg Baseball.Msg
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
  { core ∷ GenericGame model msg
  , map ∷ Model → model
  , msgmap ∷ msg → Msg
  }

data GameWrapper

gameWrap ∷ ∀ model msg. GenericGame model msg → (Model → model) → (msg → Msg) → GameWrapper
gameWrap core map msgmap = unsafeCoerce { core, map, msgmap }

gameRun ∷ ∀ r. (∀ model msg. GameWrapperF model msg → r) → GameWrapper → r
gameRun = unsafeCoerce

games ∷ Map String GameWrapper
games = Map.fromFoldable
  [ "baseball" ∧ gameWrap Baseball.game _.baseball BaseballMsg
  , "bicolor" ∧ gameWrap Bicolor.game _.bicolor BicolorMsg
  , "chocolat" ∧ gameWrap Chocolat.game _.chocolat ChocolatMsg
  , "dessin" ∧ gameWrap Dessin.game _.dessin DessinMsg
  , "eternal" ∧ gameWrap Eternal.game _.eternal EternalMsg
  , "frog" ∧ gameWrap Frog.game _.frog FrogMsg
  , "hanoi" ∧ gameWrap Hanoi.game _.hanoi HanoiMsg
  , "jetons" ∧ gameWrap Jetons.game _.jetons JetonsMsg
  , "labete" ∧ gameWrap Labete.game _.labete LabeteMsg
  , "nim" ∧ gameWrap Nim.game _.nim NimMsg
  , "noirblanc" ∧ gameWrap Noirblanc.game _.noirblanc NoirblancMsg
  , "paths" ∧ gameWrap Paths.game _.paths PathsMsg
  , "queens" ∧ gameWrap Queens.game _.queens QueensMsg
  , "roue" ∧ gameWrap Roue.game _.roue RoueMsg
  , "sansmot" ∧ gameWrap Sansmot.game _.sansmot SansmotMsg
  , "solitaire" ∧ gameWrap Solitaire.game _.solitaire SolitaireMsg
  , "tiling" ∧ gameWrap Tiling.game _.tiling TilingMsg
  , "tricolor" ∧ gameWrap Tricolor.game _.tricolor TricolorMsg
  , "" ∧ gameWrap Valise.game _.valise ValiseMsg
  ]

callByName ∷ ∀ r. String → r → (∀ model msg. GameWrapperF model msg → r) → r
callByName name default f = case games # Map.lookup name of
  Nothing → default
  Just game → game # gameRun f

update ∷ Msg → UpdateMam Model Msg Unit
update (BaseballMsg msg) = prop (Proxy ∷ Proxy "baseball") .~> mapMessage BaseballMsg (Baseball.update msg)
update (BicolorMsg msg) = prop (Proxy ∷ Proxy "bicolor") .~> mapMessage BicolorMsg (Bicolor.update msg)
update (ChocolatMsg msg) = prop (Proxy ∷ Proxy "chocolat") .~> mapMessage ChocolatMsg (Chocolat.update msg)
update (DessinMsg msg) = prop (Proxy ∷ Proxy "dessin") .~> mapMessage DessinMsg (Dessin.update msg)
update (EternalMsg msg) = prop (Proxy ∷ Proxy "eternal") .~> mapMessage EternalMsg (Eternal.update msg)
update (FrogMsg msg) = prop (Proxy ∷ Proxy "frog") .~> mapMessage FrogMsg (Frog.update msg)
update (HanoiMsg msg) = prop (Proxy ∷ Proxy "hanoi") .~> mapMessage HanoiMsg (Hanoi.update msg)
update (JetonsMsg msg) = prop (Proxy ∷ Proxy "jetons") .~> mapMessage JetonsMsg (Jetons.update msg)
update (LabeteMsg msg) = prop (Proxy ∷ Proxy "labete") .~> mapMessage LabeteMsg (Labete.update msg)
update (NimMsg msg) = prop (Proxy ∷ Proxy "nim") .~> mapMessage NimMsg (Nim.update msg)
update (NoirblancMsg msg) = prop (Proxy ∷ Proxy "noirblanc") .~> mapMessage NoirblancMsg (Noirblanc.update msg)
update (PathsMsg msg) = prop (Proxy ∷ Proxy "paths") .~> mapMessage PathsMsg (Paths.update msg)
update (QueensMsg msg) = prop (Proxy ∷ Proxy "queens") .~> mapMessage QueensMsg (Queens.update msg)
update (RoueMsg msg) = prop (Proxy ∷ Proxy "roue") .~> mapMessage RoueMsg (Roue.update msg)
update (SansmotMsg msg) = prop (Proxy ∷ Proxy "sansmot") .~> mapMessage SansmotMsg (Sansmot.update msg)
update (SolitaireMsg msg) = prop (Proxy ∷ Proxy "solitaire") .~> mapMessage SolitaireMsg (Solitaire.update msg)
update (TilingMsg msg) = prop (Proxy ∷ Proxy "tiling") .~> mapMessage TilingMsg (Tiling.update msg)
update (TricolorMsg msg) = prop (Proxy ∷ Proxy "tricolor") .~> mapMessage TricolorMsg (Tricolor.update msg)
update (ValiseMsg msg) = prop (Proxy ∷ Proxy "valise") .~> mapMessage ValiseMsg (Valise.update msg)
update Init = init
update (KeyDown k) = do
  model' ← get
  callByName model'.location (pure unit) \game →
    for_ (game.core.onKeydown k) \msg →
      update (game.msgmap msg)
update HashChanged = do
  hash ← getHash
  let location = String.drop 1 hash
  modify_ _ { location = location }
  if location == "" then
    prop (Proxy ∷ Proxy "valise") .~> mapMessage ValiseMsg (Valise.enterA)
  else
    pure unit

init ∷ UpdateMam Model Msg Unit
init = do
  _ <- Subs.onKeyDown (Just ∘ KeyDown)
  _ <- Subs.onHashChange $ const (Just HashChanged)
  for_ (Map.values games) $
    gameRun \game → case game.core.init of
      Nothing → pure unit
      Just init' → update $ game.msgmap init'
  update HashChanged

view ∷ Model → Html Msg
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
              [ S.svg [ H.style "width" "100%", H.style "height" "100%" ]
                  [ S.use [ P.href "#valise" ] ]
              ]
        , viewGame model
        ]
    ]

viewGame ∷ Model → Html Msg
viewGame model = callByName model.location H.empty
  \game → game.msgmap <$> game.core.view (game.map model)

main ∷ Effect Unit
main = do
  newSeed <- randomSeed
  genState <- Ref.new { newSeed, size: 0 }
  app
    { init: { model: appModel, msg: Just Init }
    , view
    , update: hoist (flip runReaderT { genState }) <<< update
    , selector: "#root"
    }