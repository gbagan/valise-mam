module Game where
import Effect (Effect)
import Data.Lens (Lens')
import Pha (VDom)
import Pha.Action (Action)

class CGame b where
    init :: b -> Effect b
    view :: forall a. Lens' a b -> b -> VDom a
    onKeyDown :: String -> Action b