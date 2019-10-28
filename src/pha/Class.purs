module Pha.Class where
  
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data VDom :: Type -> Type
foreign import data Event :: Type
newtype Action a = Action ((a -> Effect a) -> Event -> a -> Aff a)

data Prop a =
    Key String
  | Attr String String
  | Class String Boolean
  | Style String String
  | Event String (Action a)