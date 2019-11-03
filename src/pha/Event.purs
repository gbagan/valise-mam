module Pha.Event where
import Prelude
import Pha.Action (Action, Event, onlyEffectAction)
import Data.Maybe (Maybe(..))
import Effect (Effect)
infixr 9 compose as ∘

foreign import unsafeToMaybeAux :: forall a. Maybe a -> (a -> Maybe a) -> a -> Maybe a
unsafeToMaybe :: forall a. a -> Maybe a
unsafeToMaybe = unsafeToMaybeAux Nothing Just

foreign import shiftKey :: Event -> Boolean

foreign import unsafePointerType :: Event -> String
pointerType :: Event -> Maybe String
pointerType = unsafeToMaybe ∘ unsafePointerType

foreign import unsafeKey :: Event -> String
key :: Event -> Maybe String
key = unsafeToMaybe ∘ unsafeKey


foreign import preventDefault :: Event -> Effect Unit

preventDefaultA :: forall a. Action a
preventDefaultA = onlyEffectAction preventDefault