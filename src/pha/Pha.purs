module Pha where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Pha.Action (Action)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple)

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


foreign import appAux :: forall a effs. {
    state :: a,
    view :: a -> VDom a effs,
    node :: String,
    launchAff :: Aff a -> Effect (Fiber a),
    events :: Array (Tuple String (Action a effs)),
    init :: Action a effs
} -> Effect Unit

app :: forall a effs. {
    state :: a,
    view :: a -> VDom a effs,
    node :: String,
    events :: Array (Tuple String (Action a effs)),
    init :: Action a effs
} -> Effect Unit
app {view, node, state, events, init} = appAux {view, node, state, events, init, launchAff}
