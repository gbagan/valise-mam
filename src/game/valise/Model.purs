module Game.Valise.Model where
import Prelude
import Effect.Aff (delay)
import Data.Lens (Lens', lens, (.~), (%~))
import Pha.Action (Action, action, asyncAction)
import Data.Time.Duration (Milliseconds(..))
infixr 9 compose as ∘

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

state :: State
state = {
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


incPawPassingsA :: Action State
incPawPassingsA = action $ _pawPassings %~ \x -> min 4 (x + 1)

showHelpA :: String -> Action State
showHelpA help = action $ (_help %~ if help == "" then identity else const help) ∘ (_helpVisible .~ (help /= ""))

toggleSwitchA :: Action State
toggleSwitchA = action $ _isSwitchOn %~ not

enterA :: Action State
enterA = asyncAction \{updateState, dispatch} _ -> do
    _ <- updateState (\st -> st{
          isOpen = false,
         -- drag: null,
--        help: null,
--        position: {},
          helpVisible = false })
    delay $ Milliseconds 1500.0
    updateState (_{isOpen = true})

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