module Pha.Lens where

import Data.Lens (Lens')
import Pha (VDom)
import Pha.Action (Action, zoomAt)

foreign import viewOverAux :: ∀a b effs. (∀eff2. Action b eff2 -> Action a eff2) -> VDom b effs -> VDom a effs

viewOver :: ∀a b effs. Lens' a b -> VDom b effs -> VDom a effs
viewOver lens = viewOverAux (zoomAt lens)