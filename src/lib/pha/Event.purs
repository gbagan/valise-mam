module Pha.Event where
import Pha (Event)
import Data.Maybe (Maybe(..))

foreign import shiftKey :: Event -> Boolean

foreign import pointerTypeAux :: Maybe String -> (String -> Maybe String) -> Event -> Maybe String

pointerType :: Event -> Maybe String
pointerType = pointerTypeAux Nothing Just