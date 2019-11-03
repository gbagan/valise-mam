module Pha.Action where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Lib.Random (Random, runRnd)
import Data.Lens (Lens', view, (.~), (%~))

foreign import data Event :: Type
newtype Action a = Action (((a -> a) -> Effect a) -> Event -> Aff a)

instance semigroupAction :: Semigroup (Action a) where
    append (Action f) (Action g) = Action \dispatch e -> do
        _ <- f dispatch e
        g dispatch e

instance semigroupMonoid :: Monoid (Action a) where
    mempty = Action (\dispatch ev -> liftEffect (dispatch identity))

action :: forall a. (a -> a) -> Action a
action fn = Action $ \dispatch e -> liftEffect $ dispatch fn

randomAction :: forall a. (a -> Random a) -> Action a
randomAction fn = Action $ \dispatch ev -> liftEffect do
    st <- dispatch identity
    st2 <- runRnd (fn st)
    dispatch (const st2)

type AsyncHelpers a = {
    getState :: Aff a,
    setState :: a -> Aff a,
    updateState :: (a -> a) -> Aff a,
    dispatch :: Action a -> Aff a
}

asyncAction :: forall a. (AsyncHelpers a -> a -> Aff a) -> Action a
asyncAction act = Action \dispatch e -> do
    st <- liftEffect (dispatch identity)
    act {
        getState: liftEffect $ dispatch identity,
        setState: \x -> liftEffect $ dispatch (const x),
        updateState: \fn -> liftEffect $ dispatch fn,
        dispatch: \(Action a) -> a dispatch e
    } st

lensAction :: forall a b. Lens' a b -> Action b -> Action a
lensAction lens (Action act) = Action \dispatch ev -> do
    subst <- act (\fn -> dispatch (lens %~ fn) <#> view lens) ev 
    st <- liftEffect (dispatch identity)
    pure $ st # lens .~ subst

infixl 3  lensAction as 🎲

ifThenElseA :: forall a. (a -> Event -> Boolean) -> Action a -> Action a -> Action a
ifThenElseA cond (Action action1) (Action action2) = Action $ \dispatch ev -> do
    st <- liftEffect $ dispatch identity
    (if cond st ev then action1 else action2) dispatch ev

whenA :: forall a. (a -> Event -> Boolean) -> Action a -> Action a
whenA cond act = ifThenElseA cond act mempty

withPayload :: forall a b. (b -> Action a) -> (Event -> b) -> Action a
withPayload act payloadFn = Action \dispatch e -> do
    let (Action a) = act (payloadFn e)
    a dispatch e

withPayload' :: forall a b. (b -> Action a) -> (Event -> Effect b) -> Action a
withPayload' act payloadFn = Action \dispatch e -> do
    payload <- liftEffect $ payloadFn e
    let (Action a) = act payload
    a dispatch e