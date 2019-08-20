module UI.Icons where
import Prelude
import Data.Array (null)
import Pha (VDom, Prop, text)
import Pha.Html (div', click, class')
import Lib.Game (State(St), class Game, undo, redo, reset, setDialog, Dialog(Rules))
import UI.Icon (iconbutton, Options, Icon(IconSymbol)) as I

type LensAction a b = (b -> b) -> a -> a

iconbutton :: forall a b c d e. Game a b c d =>
    State a b c 
    -> (I.Options -> I.Options) 
    -> Array (Prop e)
    -> VDom e
iconbutton (St st) optionFn props = I.iconbutton optionFn props

iundo :: forall a b c d e.  Game a b c d =>
    LensAction e (State a b c) -> State a b c -> VDom e
iundo action (St st) =
    iconbutton
        (St st)
        (_{icon = I.IconSymbol "#undo", disabled = null st.history})
        [click $ action undo]

iredo :: forall a b c d e.  Game a b c d =>
    LensAction e (State a b c) -> State a b c -> VDom e
iredo action (St st) =
    iconbutton
        (St st)
        (_{icon = I.IconSymbol "#undo", disabled = null st.history})
        [click $ action redo]

ireset :: forall a b c d e.  Game a b c d =>
    LensAction e (State a b c) -> State a b c -> VDom e
ireset action (St st) =
    iconbutton
        (St st)
        (_{icon = I.IconSymbol "#reset", disabled = null st.history})
        [click $ action reset]

irules :: forall a b c d e.  Game a b c d =>
    LensAction e (State a b c) -> State a b c -> VDom e
irules action (St st) =
    iconbutton
        (St st)
        (_{icon = I.IconSymbol "#rules", selected = selected})
        [click $ action $ setDialog Rules]
    where
        selected = case st.dialog of
            Rules -> true
            _ -> false

winPanel :: forall a b c e. State a b c -> VDom e
winPanel (St {levelFinished}) =
    div' [class' "ui-flex-center ui-absolute component-win-container" true] [
        div' [class' "component-win" true, class' "visible" levelFinished] [
            text "GAGNÉ"
        ]
    ]
