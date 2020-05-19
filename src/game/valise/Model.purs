module Game.Valise.Model where
import MyPrelude
import Pha.Update (Update, put, modify)
import Data.Map (Map, empty) as M
import Game.Effs (EFFS, DELAY, delay)

type State = 
    {   isOpen ∷ Boolean
    -- linksAreActive ∷ Boolean, -- utile?
    ,   help ∷ String --- Maybe?
    ,   helpVisible ∷ Boolean
    ,   drag ∷ Maybe { name ∷ String, x ∷ Number, y ∷ Number }
    ,   positions ∷ M.Map String {x ∷ Number, y ∷ Number}
    ,   isSwitchOn ∷ Boolean
    }

istate ∷ State
istate = 
    {   isOpen: false
    -- ,   linksAreActive: false
    ,   help: ""
    ,   helpVisible: false
    ,   drag: Nothing
    ,   positions: M.empty
    ,   isSwitchOn: false
    }

_help ∷ Lens' State String
_help = lens _.help _{help = _}

_helpVisible ∷ Lens' State Boolean
_helpVisible = lens _.helpVisible _{helpVisible = _}

_positions ∷ Lens' State (M.Map String {x ∷ Number, y ∷ Number})
_positions = lens _.positions _{positions = _}

_drag ∷ Lens' State (Maybe { name ∷ String, x ∷ Number, y ∷ Number })
_drag = lens _.drag _{drag = _}

_isSwitchOn ∷ Lens' State Boolean
_isSwitchOn = lens _.isSwitchOn _{isSwitchOn = _}

enterA ∷ ∀effs. Update State (delay ∷ DELAY | effs) 
enterA = do
    put istate
    delay 1500
    modify _{isOpen = true}

data Msg = ShowHelp String | ToggleSwitch | SetDrag (Maybe { name ∷ String, x ∷ Number, y ∷ Number })
          | MoveObject {x ∷ Number, y ∷ Number}

update ∷ Msg → Update State EFFS
update (ShowHelp help) = modify $ (_help %~ if help == "" then identity else const help) >>> (_helpVisible .~ (help /= ""))
update ToggleSwitch = modify $ _isSwitchOn %~ not
update (SetDrag d) = modify _{drag = d}
update (MoveObject {x, y}) = modify \state →
    case state.drag of
        Just {name, x: x2, y: y2} → state # _positions ∘ at name .~ Just {x: x-x2, y: y-y2} 
        _ → state
