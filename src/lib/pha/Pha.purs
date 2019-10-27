module Pha where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Effect.Class (liftEffect)
import Data.Lens (Lens', (^.), (.~), set)
import Lib.Random (Random, runRnd)

foreign import data VDom :: Type -> Type
foreign import data Event :: Type

newtype Action a = Action ((a -> Effect a) -> Event -> a -> Aff a)

unwrapA :: forall a. Action a -> ((a -> Effect a) -> Event -> a -> Aff a)
unwrapA (Action a) = a

{-
class ClsAction st act | act -> st where 
    action :: act -> Action st
-}
action :: forall a. (a -> a) -> Action a
action fn = Action $ \setState ev st -> liftEffect $ setState $ fn st

randomAction :: forall a. (a -> Random a) -> Action a
randomAction fn = Action $ \setState ev st -> do
    st' <- liftEffect $ runRnd (fn st)
    liftEffect $ setState st'

lensAction :: forall a b. Lens' a b -> Action b -> Action a
lensAction lens (Action act) = Action \setState ev st -> do
    st2 <- act (\st' -> (setState $ set lens st' st) >>= \_ -> pure st') ev (st^.lens)
    pure $ st # lens .~ st2

infixl 3  lensAction as 🎲

noAction :: forall a. Action a
noAction = Action (\setState ev st -> pure st)

--combineA :: forall a act1 act2. ClsAction a act1 => ClsAction a act2 =>
--    act1 -> act2 -> Action a

ifThenElseA :: forall a. (a -> Event -> Boolean) -> Action a -> Action a -> Action a
ifThenElseA cond (Action action1) (Action action2) = Action $ \setState ev st ->
    (if cond st ev then action1 else action2) setState ev st

whenA :: forall a. (a -> Event -> Boolean) -> Action a -> Action a
whenA cond act = ifThenElseA cond act noAction
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
