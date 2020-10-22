module UI.Icon where

import MyPrelude
import Data.Tuple (uncurry)
import Pha as H
import Pha.Elements as HH
import Pha.Attributes as P
import Pha.Events as E
import Lib.Util (partialUpdate, class PartialRecord)

data Icon = IconText String | IconSymbol String | IconNone

type Options = (
    icon ∷ Icon,
    selected ∷ Boolean,
    tooltip ∷ Maybe String,
    round ∷ Boolean,
    large ∷ Boolean,
    hidden ∷ Boolean,
    disabled ∷ Boolean,
    style ∷ Array (Tuple String String)
)

defaultOptions ∷ Record Options
defaultOptions = {
    icon: IconNone,
    selected: false,
    tooltip: Nothing,
    round: false,
    large: false,
    hidden: false,
    disabled: false,
    style: []
}

iconbutton ∷ ∀a opts. PartialRecord opts Options ⇒ Record opts → Array (H.Prop a) → H.VDom a
iconbutton opts props =
    let {icon, selected, tooltip, round, large, hidden, disabled: d, style: st} = partialUpdate opts defaultOptions in
    HH.button (
        [   H.class_ "ui-icon"
        ,   H.class' "selected" selected
        ,   H.class' "round" large
        ,   H.class' "hidden" hidden
        ,   P.disabled d
        ,   E.preventDefaultOn "contextmenu" $ E.always (Nothing /\ true)
    ] <> props) $ [
        case icon of
            IconSymbol symbol → HH.svg ((uncurry H.style <$> st) <> [P.width "100%", P.height "100%"]) [
                HH.use [P.href symbol, H.class_ "ui-icon-symbol"]
            ]
            IconText t → HH.span [H.class_ "ui-icon-text"] [H.text t]
            IconNone → H.emptyNode,
        H.maybe tooltip \t → HH.span [H.class_ "ui-icon-tooltip"] [H.text t]
    ]
