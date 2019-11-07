module Pha where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Pha.Action (Action, Event, GetStateF(..), SetStateF(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple)
import Run (VariantF, runCont, onMatch, match)
import Prim.RowList (class RowToList)
import Prim.Row (class Union)
import Data.Variant.Internal (class VariantFMatchCases)

foreign import data VDom :: Type -> #Type -> Type

data Prop st effs =
    Key String
  | Attr String String
  | Class String Boolean
  | Style String String
  | Event String (Action st effs)

isStyle :: forall st effs. Prop st effs -> Boolean
isStyle (Style _ _) = true
isStyle _ = false

foreign import hAux :: forall st effs. (Prop st effs -> Boolean) -> String -> Array (Prop st effs) -> Array (VDom st effs) -> VDom st effs
h :: forall st effs. String -> Array (Prop st effs) -> Array (VDom st effs) -> VDom st effs
h = hAux isStyle

foreign import text :: forall a effs. String -> VDom a effs

foreign import emptyNode :: forall a effs. VDom a effs

whenN :: forall a effs. Boolean -> (Unit -> VDom a effs) -> VDom a effs
whenN cond vdom = if cond then vdom unit else emptyNode

maybeN :: forall a effs. Maybe (VDom a effs) -> VDom a effs
maybeN = fromMaybe emptyNode

-- type Match effs = -- forall rl r1 r2 a. RowToList effs rl => VariantFMatchCases rl r1 a (Effect Unit) => Union r1 () r2 =>
--   Record effs -- -> VariantF r2 a -> b} -> Effect Unit
type Match effs = forall b. VariantF effs (Effect Unit) -> Effect Unit

type Dispatch = forall st effs. Effect st -> ((st -> st) -> Effect Unit) -> Match effs -> Action st effs -> Effect Unit
dispatch :: Dispatch
dispatch getS setS matching = runCont go (\_ -> pure unit) where
    go = onMatch {
        getState: \(GetState cont) -> getS >>= cont,
        setState: \(SetState fn cont) -> setS fn *> cont
    } matching


foreign import appAux :: forall a effs. Dispatch -> {
    state :: a,
    view :: a -> VDom a effs,
    node :: String,
    events :: Array (Tuple String (Action a effs)),
    init :: Action a effs,
    effects :: Event -> Match effs
} -> Effect Unit

app :: forall a effs. {
    state :: a,
    view :: a -> VDom a effs,
    node :: String,
    events :: Array (Tuple String (Action a effs)),
    init :: Action a effs,
    effects :: Event -> Match effs
} -> Effect Unit
app = appAux dispatch
