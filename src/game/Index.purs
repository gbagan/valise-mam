module Game where
import Effect (Effect)
import Data.Lens (Lens')
import Pha (VDom)
import Game.Types (EFFS)
import Pha.Action (Action)

class CGame b where
    init :: b -> Effect b
    view :: forall a. Lens' a b -> b -> VDom a EFFS
    onKeyDown :: String -> Action b EFFS