module Game.Valise.Model where
import MyPrelude
import Pha.Action (Action, action, DELAY, delay, setState)

type State = {
    isOpen :: Boolean,
    -- linksAreActive :: Boolean, -- utile?
    help :: String, --- Maybe?
    helpVisible :: Boolean,
    -- drag: null,
    -- position: {},
    isSwitchOn :: Boolean,
    pawPassings :: Int
}

istate :: State
istate = {
    isOpen: false,
    -- linksAreActive: false,
    help: "",
    helpVisible: false,
    -- drag: null,
    -- position: {},
    isSwitchOn: false,
    pawPassings: 0
}

_help :: Lens' State String
_help = lens (_.help) (_{help = _})
_helpVisible :: Lens' State Boolean
_helpVisible = lens (_.helpVisible) (_{helpVisible = _})
_pawPassings :: Lens' State Int
_pawPassings = lens (_.pawPassings) (_{pawPassings = _})
_isSwitchOn :: Lens' State Boolean
_isSwitchOn = lens (_.isSwitchOn) (_{isSwitchOn = _})

incPawPassingsA :: ∀effs. Action State effs
incPawPassingsA = action $ _pawPassings %~ \x -> min 4 (x + 1)

showHelpA :: ∀effs. String -> Action State effs
showHelpA help = action $ (_help %~ if help == "" then identity else const help) ∘ (_helpVisible .~ (help /= ""))

toggleSwitchA :: ∀effs. Action State effs
toggleSwitchA = action $ _isSwitchOn %~ not

enterA :: ∀effs. Action State (delay :: DELAY | effs) 
enterA = do
    delay 1500
    setState (_{isOpen = true})

leaveA :: ∀effs. Action State effs 
leaveA = setState (\st -> st{
    isOpen = false,
     -- drag: null,
--        help: null,
--        position: {},
    helpVisible = false
})

{-
--const actions = {

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