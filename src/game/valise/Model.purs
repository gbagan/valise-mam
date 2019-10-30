module Game.Valise.Model where
import Prelude

import Data.Lens (Lens', lens, (.~), (%~))
import Pha.Class (Action)
import Pha.Action (action)

type ValiseState = {
    isOpen :: Boolean,
    linksAreActive :: Boolean, -- utile?
    help :: String, --- Maybe?
    helpVisible :: Boolean,
    -- drag: null,
    -- position: {},
    isSwitchOn :: Boolean,
    pawPassings :: Int
}

valiseState :: ValiseState
valiseState = {
    isOpen: false,
    linksAreActive: false,
    help: "",
    helpVisible: false,
    -- drag: null,
    -- position: {},
    isSwitchOn: false,
    pawPassings: 0
}

_help :: Lens' ValiseState String
_help = lens (_.help) (_{help = _})
_helpVisible :: Lens' ValiseState Boolean
_helpVisible = lens (_.helpVisible) (_{helpVisible = _})
_pawPassings :: Lens' ValiseState Int
_pawPassings = lens (_.pawPassings) (_{pawPassings = _})
_isSwitchOn :: Lens' ValiseState Boolean
_isSwitchOn = lens (_.isSwitchOn) (_{isSwitchOn = _})


incPawPassingsA :: Action ValiseState
incPawPassingsA = action $ _pawPassings %~ \x -> min 4 (x + 1)

openValiseA :: Action ValiseState
openValiseA = action $ _{isOpen = true}

showHelpA :: String -> Action ValiseState
showHelpA help = action $ (_help %~ if help == "" then identity else const help) <<< (_helpVisible .~ (help /= ""))

toggleSwitchA :: Action ValiseState
toggleSwitchA = action $ _isSwitchOn %~ not

{-
--const actions = {
--    enter: state => [{
--        ...state,
--        isOpen: false,
--        drag: null,
--        help: null,
--        helpVisible: false,
--        position: {},
--   }, timeout(openValise, 1500)
--    ],
 
incPawPassings = action $ _pawPassings %~ \x -> min 4 (x + 1),
--    beginDrag: update('drag'),

    moveObject: (state, position) => !state.drag || !position ? state
        : state |> set(['position', state.drag.name], {
            left: position.left - state.drag.left,
            top: position.top - state.drag.top,
        }),

    finishDrag: set('drag', null),

    showHelp: update(help => ({help: or(help), helpVisible: !!help})), 

    toggleSwitch: set('isSwitchOn', not),