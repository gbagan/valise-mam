module UI.Icons where
import Prelude
import Data.Array (null)
import Optic.Core (Lens', over)
import Pha (VDom, Prop)
import Pha.Html (click)
import Lib.Game (State(St), class Game, undo, redo, reset)
import UI.Icon (iconbutton, Options, Icon(IconSymbol)) as I

iconbutton :: forall a b c d e. Game a b c d =>
    State a b c 
    -> (I.Options -> I.Options) 
    -> Array (Prop e)
    -> VDom e
iconbutton (St st) optionFn props = I.iconbutton optionFn props

iundo :: forall a b c d e.  Game a b c d =>
    Lens' e (State a b c) -> State a b c -> VDom e
iundo lens (St st) =
    iconbutton
        (St st)
        (_{icon = I.IconSymbol "#undo", disabled = null st.history})
        [click $ over lens undo]

iredo :: forall a b c d e.  Game a b c d =>
    Lens' e (State a b c) -> State a b c -> VDom e
iredo lens (St st) =
    iconbutton
        (St st)
        (_{icon = I.IconSymbol "#redo", disabled = null st.history})
        [click $ over lens redo]

ireset :: forall a b c d e.  Game a b c d =>
    Lens' e (State a b c) -> State a b c -> VDom e
ireset lens (St st) =
    iconbutton
        (St st)
        (_{icon = I.IconSymbol "#reset", disabled = null st.history})
        [click $ over lens reset]
