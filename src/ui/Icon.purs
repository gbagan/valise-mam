module UI.Icon where

import Prelude
import Data.Maybe (Maybe (Nothing), maybe)
import Data.Tuple (Tuple(..))
import Pha (text, emptyNode)
import Pha.Class (VDom, Prop) 
import Pha.Html (button, div', span, svguse, class', h2, style, disabled)

data Icon = IconText String | IconSymbol String | IconNone

type Options = {
    icon :: Icon,
    selected :: Boolean,
    tooltip :: Maybe String,
    round :: Boolean,
    large :: Boolean,
    hidden :: Boolean,
    disabled :: Boolean,
    style :: Array (Tuple String String)
}

defaultOptions :: Options
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

iconbutton :: forall a. (Options -> Options) -> Array (Prop a) -> VDom a
iconbutton optionFn props =
    let {icon, selected, tooltip, round, large, hidden, disabled: d, style: st} = optionFn defaultOptions in
    button ([
        class' "ui-icon" true,
        class' "selected" selected,
        class' "round" large,
        class' "hidden" hidden,
        disabled d
    ] <> props) [
        case icon of
            IconSymbol symbol -> svguse symbol $ [class' "ui-icon-symbol" true] <> (st <#> \(Tuple k v) -> style k v)
            IconText t -> span [class' "ui-icon-text" true] [text t]
            IconNone -> emptyNode,
        tooltip # maybe emptyNode (\t -> span [class' "ui-icon-tooltip" true] [text t])
    ]

icongroup :: forall a. String -> Array (VDom a) -> VDom a
icongroup title children =
    div' [] [
        h2 [] [text title],
        div' [class' "ui-icon-grid" true] children
    ]