module Pha.Event where
import Prelude
import Pha.Class (Event)
import Data.Maybe (Maybe(..))

foreign import unsafeToMaybeAux :: forall a. Maybe a -> (a -> Maybe a) -> a -> Maybe a
unsafeToMaybe :: forall a. a -> Maybe a
unsafeToMaybe = unsafeToMaybeAux Nothing Just

foreign import shiftKey :: Event -> Boolean

foreign import unsafePointerType :: Event -> String
pointerType :: Event -> Maybe String
pointerType = unsafeToMaybe <<< unsafePointerType

foreign import unsafeKey :: Event -> String
key :: Event -> Maybe String
key = unsafeToMaybe <<< unsafeKey