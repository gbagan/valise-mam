module UI.Icons where
import Prelude
import Data.Array (null)
import Optic.Core (Lens', (^.), (.~))
import Pha (VDom, Prop, text, Action, action, lensAction)
import Pha.Html (div', click, class')
import Lib.Game (State, class Game, undo, redo, reset, Dialog(Rules), _dialog, _history, _levelFinished)
import UI.Icon (iconbutton, Options, Icon(IconSymbol)) as I

type LensAction a b = (b -> b) -> Action a

iconbutton :: forall a b c d. Game a b c =>
    State a b
    -> (I.Options -> I.Options) 
    -> Array (Prop d)
    -> VDom d
iconbutton state optionFn props = I.iconbutton optionFn props

iundo :: forall a b c d.  Game a b c => Lens' d (State a b) -> State a b -> VDom d
iundo lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#undo", disabled = null $ state^._history})
        [click $ lensAction lens $ action undo]

iredo :: forall a b c d.  Game a b c => Lens' d (State a b) -> State a b -> VDom d
iredo lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#undo", disabled = null $ state^._history})
        [click $ lensAction lens $ action redo]

ireset :: forall a b c d.  Game a b c => Lens' d (State a b) -> State a b -> VDom d
ireset lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#reset", disabled = null $ state^._history})
        [click $ lensAction lens $ action reset]

irules :: forall a b c d.  Game a b c => Lens' d (State a b) -> State a b -> VDom d
irules lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#rules", selected = selected})
        [click $ lensAction lens $ action $ _dialog .~ Rules]
    where
        selected = case state^._dialog of
            Rules -> true
            _ -> false

winPanel :: forall a b d. State a b -> VDom d
winPanel state =
    div' [class' "ui-flex-center ui-absolute component-win-container" true] [
        div' [class' "component-win" true, class' "visible" $ state^._levelFinished] [
            text "GAGNÉ"
        ]
    ]
