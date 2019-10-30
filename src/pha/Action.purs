module Pha.Action where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Lib.Random (Random, runRnd)
import Data.Lens (Lens', set, (^.), (.~))

foreign import data Event :: Type
newtype Action a = Action ((a -> Effect a) -> Event -> a -> Aff a)

instance semigroupAction :: Semigroup (Action a) where
    append (Action f) (Action g) = Action \setState e state -> do
        st2 <- f setState e state
        g setState e st2

unwrapA :: forall a. Action a -> ((a -> Effect a) -> Event -> a -> Aff a)
unwrapA (Action a) = a

action :: forall a. (a -> a) -> Action a
action fn = Action $ \setState ev st -> liftEffect $ setState $ fn st

randomAction :: forall a. (a -> Random a) -> Action a
randomAction fn = Action $ \setState ev st -> liftEffect $ setState =<< runRnd (fn st)

lensAction :: forall a b. Lens' a b -> Action b -> Action a
lensAction lens (Action act) = Action \setState ev st -> do
    st2 <- act (\st' -> (setState $ set lens st' st) <#> const st') ev (st^.lens)
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