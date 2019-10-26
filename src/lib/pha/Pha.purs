module Pha where

import Prelude
import Effect (Effect)
import Optic.Core (Lens', (^.), set)
import Lib.Random (RandomFn(..), runRnd)

foreign import data VDom :: Type -> Type
foreign import data Event :: Type

newtype Action a = Action ((a -> Effect Unit) -> Event -> a -> Effect Unit)

class ClsAction st act | act -> st where 
    action :: act -> Action st

instance lensactionState :: ClsAction a (a -> a) where 
    action fn = Action (\setState ev st -> setState $ fn st)

instance lensactionrnd :: ClsAction a (RandomFn a) where
    action (RandomFn fn) = Action (\setState ev st -> runRnd (fn st) >>= setState)

instance lensactionId :: ClsAction a (Action a) where 
    action = identity

lensAction :: forall a b act. ClsAction b act => Lens' a b -> act -> Action a
lensAction lens act = Action \setState ev st -> act' (\st' -> setState $ set lens st' st) ev (st^.lens)
    where Action act' = action act

infixl 3  lensAction as 🎲

ifThenElseA :: forall a act1 act2. ClsAction a act1 => ClsAction a act2 =>
        (Event -> Boolean) -> act1 -> act2 -> Action a
ifThenElseA cond action1 action2 = Action (\setState ev st ->
    let Action act = if cond ev then action action1 else action action2 in act setState ev st
)

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

foreign import app :: forall a. {
    init :: a,
    view :: a -> VDom a,
    node :: String
} -> Effect Unit
