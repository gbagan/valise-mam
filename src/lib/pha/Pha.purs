module Pha where

import Prelude
import Effect (Effect)
import Optic.Core (Lens', (^.), set)
import Lib.Random (Random, runRnd)

foreign import data VDom :: Type -> Type

type Action a = (a -> Effect a) -> Unit -> a -> Effect Unit

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
action fn setState _ st = do
    _ <- setState $ fn st
    pure unit

rndAction :: forall a. (a -> Random a) -> Action a
rndAction fn setState _ st = do
    st' <- runRnd $ fn st
    _ <- setState $ st'
    pure unit

lensAction :: forall a b. Lens' a b -> Action b -> Action a
lensAction lens act setState ev st = act (\st' -> do
    _ <- setState $ set lens st' st
    pure st'
) ev (st ^. lens)

foreign import app :: forall a. {
    init :: a,
    view :: a -> VDom a,
    node :: String
} -> Effect Unit
