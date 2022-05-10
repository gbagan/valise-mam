module Game.Valise.Model where

import MamPrelude

import Data.Map as Map
import Game.Common (pointerDecoder)
import Lib.Update (UpdateMam, delay)
import Web.PointerEvent (PointerEvent)

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
_help = prop (Proxy ∷ _ "help")
_helpVisible ∷ Lens' State Boolean
_helpVisible = prop (Proxy ∷ _ "helpVisible")
_positions ∷ Lens' State (Map String {x ∷ Number, y ∷ Number})
_positions = prop (Proxy ∷ _ "positions")
_drag ∷ Lens' State (Maybe { name ∷ String, x ∷ Number, y ∷ Number })
_drag = prop (Proxy ∷ _ "drag")
_isSwitchOn ∷ Lens' State Boolean
_isSwitchOn = prop (Proxy ∷ _ "isSwitchOn")

enterA ∷ UpdateMam State
enterA = do
    put istate
    delay (Milliseconds 1500.0)
    modify_ _{isOpen = true}

data Msg = ShowHelp String
        | ToggleSwitch
        | SetDrag (Maybe { name ∷ String, x ∷ Number, y ∷ Number })
        | MoveObject PointerEvent
        | NoAction

update ∷ Msg → UpdateMam State
update (ShowHelp help) = modify_ $ over _help (if help == "" then identity else const help)
                                >>> set _helpVisible (help ≠ "")
update ToggleSwitch = _isSwitchOn %= not
update (SetDrag d) = _drag .= d
update (MoveObject ev) = do
    pos ← liftEffect $ pointerDecoder ev
    st ← get
    case pos, st.drag of
        Just {x, y}, Just {name, x: x2, y: y2} →
            _positions ∘ at name .= Just {x: x-x2, y: y-y2} 
        _, _ → pure unit
update NoAction = pure unit