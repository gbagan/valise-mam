module Pha where

import Prelude
import Effect (Effect)

foreign import data VDom :: Type -> Type

type Action a = a -> a

data Prop a =
      Key String
    | Attr String String
    | Class String Boolean
    | Style String String
    | Event String (Action a)

isStyle :: forall a. Prop a -> Boolean
isStyle (Style _ _) = true
isStyle _ = false

foreign import hAux :: forall a. (Prop a -> Boolean) -> String -> Array (Prop a) -> Array (VDom a) -> VDom a
h :: forall a. String -> Array (Prop a) -> Array (VDom a) -> VDom a
h = hAux isStyle

foreign import text :: forall a. String -> VDom a

foreign import emptyNode :: forall a. VDom a

action :: forall a. (a -> a) -> Action a
action fn = fn

foreign import app :: forall a. {
    init :: Unit -> a,
    view :: a -> VDom a,
    node :: String
} -> Effect Unit
