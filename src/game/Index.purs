module Game where
import Data.Lens (Lens')
import Pha (VDom)
import Game.Effs (EFFS)
import Pha.Action (Action)

class CGame a msg | a -> msg where
    init :: Action a EFFS
    view :: a -> VDom msg
    onKeyDown :: String -> Action a EFFS