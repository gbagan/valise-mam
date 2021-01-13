module Game.Valise.Model where
import MyPrelude
import Lib.Update (Update, put, modify_, delay)
import Data.Map as Map

type State = 
    {   isOpen ∷ Boolean
    -- linksAreActive ∷ Boolean, -- utile?
    ,   help ∷ String --- Maybe?
    ,   helpVisible ∷ Boolean
    ,   drag ∷ Maybe { name ∷ String, x ∷ Number, y ∷ Number }
    ,   positions ∷ Map String {x ∷ Number, y ∷ Number}
    ,   isSwitchOn ∷ Boolean
    }

istate ∷ State
istate = 
    {   isOpen: false
    -- ,   linksAreActive: false
    ,   help: ""
    ,   helpVisible: false
    ,   drag: Nothing
    ,   positions: Map.empty
    ,   isSwitchOn: false
    }

_help ∷ Lens' State String
_help = prop (SProxy ∷ _ "help")
_helpVisible ∷ Lens' State Boolean
_helpVisible = prop (SProxy ∷ _ "helpVisible")
_positions ∷ Lens' State (Map String {x ∷ Number, y ∷ Number})
_positions = prop (SProxy ∷ _ "positions")
_drag ∷ Lens' State (Maybe { name ∷ String, x ∷ Number, y ∷ Number })
_drag = prop (SProxy ∷ _ "drag")
_isSwitchOn ∷ Lens' State Boolean
_isSwitchOn = prop (SProxy ∷ _ "isSwitchOn")

enterA ∷ Update State Unit
enterA = do
    put istate
    delay (Milliseconds 1500.0)
    modify_ _{isOpen = true}

data Msg = ShowHelp String | ToggleSwitch | SetDrag (Maybe { name ∷ String, x ∷ Number, y ∷ Number })
          | MoveObject {x ∷ Number, y ∷ Number}

update ∷ Msg → Update State Unit
update (ShowHelp help) = modify_ $ over _help (if help == "" then identity else const help)
                                >>> set _helpVisible (help ≠ "")
update ToggleSwitch = _isSwitchOn %= not
update (SetDrag d) = _drag .= d
update (MoveObject {x, y}) = modify_ \state →
    case state.drag of
        Just {name, x: x2, y: y2} → state # set (_positions ∘ at name) (Just {x: x-x2, y: y-y2}) 
        _ → state
