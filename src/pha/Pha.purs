module Pha where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Pha.Class (VDom, Prop(..))
import Data.Maybe (Maybe, fromMaybe)

isStyle :: forall a. Prop a -> Boolean
isStyle (Style _ _) = true
isStyle _ = false

foreign import hAux :: forall a. (Prop a -> Boolean) -> String -> Array (Prop a) -> Array (VDom a) -> VDom a
h :: forall a. String -> Array (Prop a) -> Array (VDom a) -> VDom a
h = hAux isStyle

foreign import text :: forall a. String -> VDom a

foreign import emptyNode :: forall a. VDom a

whenN :: forall a. Boolean -> (Unit -> VDom a) -> VDom a
whenN cond vdom = if cond then vdom unit else emptyNode

maybeN :: forall a. Maybe (VDom a) -> VDom a
maybeN = fromMaybe emptyNode

foreign import appAux :: forall a. {
    init :: a,
    view :: a -> VDom a,
    node :: String,
    launchAff :: Aff a -> Effect (Fiber a)
} -> Effect Unit

app :: forall a. {
    init :: a,
    view :: a -> VDom a,
    node :: String
} -> Effect Unit
app {init, view, node} = appAux {init, view, node, launchAff}
