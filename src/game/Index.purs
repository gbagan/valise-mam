module Game where
import Data.Lens (Lens')
import Pha (VDom)
import Game.Effs (EFFS)
import Pha.Action (Action)

class CGame b where
    init :: Action b EFFS
    view :: ∀a. Lens' a b -> b -> VDom a EFFS
    onKeyDown :: String -> Action b EFFS