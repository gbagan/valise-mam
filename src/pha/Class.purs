module Pha.Class where

import Effect (Effect)
import Effect.Aff (Aff)
import Pha.Action (Action)

foreign import data VDom :: Type -> Type

data Prop a =
    Key String
  | Attr String String
  | Class String Boolean
  | Style String String
  | Event String (Action a)