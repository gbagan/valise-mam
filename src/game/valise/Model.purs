module Game.Valise.Model where
import MyPrelude
import Pha (Event) 
import Pha.Action (Action, DELAY, delay, getState, setState)
import Game.Effs (getPointerPosition, POINTER)
import Data.Map (Map, empty) as M

type State = {
    isOpen :: Boolean,
    -- linksAreActive :: Boolean, -- utile?
    help :: String, --- Maybe?
    helpVisible :: Boolean,
    drag :: Maybe { name :: String, x :: Number, y :: Number },
    positions :: M.Map String {x :: Number, y :: Number},
    isSwitchOn :: Boolean
}

istate :: State
istate = {
    isOpen: false,
    -- linksAreActive: false,
    help: "",
    helpVisible: false,
    drag: Nothing,
    positions: M.empty,

    isSwitchOn: false
}

_help :: Lens' State String
_help = lens _.help _{help = _}

_helpVisible :: Lens' State Boolean
_helpVisible = lens _.helpVisible _{helpVisible = _}

_positions :: Lens' State (M.Map String {x :: Number, y :: Number})
_positions = lens _.positions _{positions = _}

_drag :: Lens' State (Maybe { name :: String, x :: Number, y :: Number })
_drag = lens _.drag _{drag = _}

_isSwitchOn :: Lens' State Boolean
_isSwitchOn = lens _.isSwitchOn _{isSwitchOn = _}

showHelpA :: ∀effs. String -> Action State effs
showHelpA help = setState $ (_help %~ if help == "" then identity else const help) ∘ (_helpVisible .~ (help /= ""))

toggleSwitchA :: ∀effs. Action State effs
toggleSwitchA = setState (_isSwitchOn %~ not)

enterA :: ∀effs. Action State (delay :: DELAY | effs) 
enterA = do
    delay 1500
    setState _{isOpen = true}

leaveA :: ∀effs. Action State effs 
leaveA = setState (\_ -> istate)

setDragA :: ∀effs. Maybe { name :: String, x :: Number, y :: Number } -> Action State effs
setDragA d = setState _{drag = d}

moveObjectA :: ∀effs. Event -> Action State (pointer :: POINTER | effs)
moveObjectA ev = do
    state <- getState
    pos <- getPointerPosition ev
    case state.drag ∧ pos of
        Just {name, x: x2, y: y2} ∧ Just {x, y} -> setState $ _positions ∘ at name .~ Just {x: x-x2, y:y-y2}
        _ -> pure unit
