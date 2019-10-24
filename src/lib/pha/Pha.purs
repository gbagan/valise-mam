module Pha where

import Prelude
import Effect (Effect)
import Optic.Core (Lens', (^.), set)
import Lib.Random (Random, runRnd)

foreign import data VDom :: Type -> Type

type Action a = (a -> Effect Unit) -> a -> Effect Unit

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
action = (>>>)

rndAction :: forall a. (a -> Random a) -> Action a
rndAction fn setState st = runRnd (fn st) >>= setState

lensAction :: forall a b. Lens' a b -> Action b -> Action a
lensAction lens act setState st = act (\st' -> setState $ set lens st' st) (st ^. lens)

foreign import app :: forall a. {
    init :: a,
    view :: a -> VDom a,
    node :: String
} -> Effect Unit
