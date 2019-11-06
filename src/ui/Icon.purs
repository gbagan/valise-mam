module UI.Icon where

import MyPrelude
import Data.Tuple (uncurry)
import Pha (VDom, Prop, text, emptyNode, maybeN)
import Pha.Html (button, span, svguse, class', style, disabled)

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

iconbutton :: forall a effs. (Options -> Options) -> Array (Prop a effs) -> VDom a effs
iconbutton optionFn props =
    let {icon, selected, tooltip, round, large, hidden, disabled: d, style: st} = optionFn defaultOptions in
    button ([
        class' "ui-icon" true,
        class' "selected" selected,
        class' "round" large,
        class' "hidden" hidden,
        disabled d
    ] <> props) $ [
        case icon of
            IconSymbol symbol -> svguse symbol $ [class' "ui-icon-symbol" true] <> (st <#> uncurry style)
            IconText t -> span [class' "ui-icon-text" true] [text t]
            IconNone -> emptyNode,
        maybeN $ tooltip <#> \t -> span [class' "ui-icon-tooltip" true] [text t]
    ]
