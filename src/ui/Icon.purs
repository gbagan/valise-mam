module UI.Icon where

import MyPrelude
import Data.Tuple (uncurry)
import Pha (VDom, Prop, text, emptyNode, maybeN, class_, class', style)
import Pha.Elements (button, span)
import Pha.Attributes (disabled)
import Pha.Svg (svg, use, width, height)

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

iconbutton :: ∀a effs. (Options -> Options) -> Array (Prop a effs) -> VDom a effs
iconbutton optionFn props =
    let {icon, selected, tooltip, round, large, hidden, disabled: d, style: st} = optionFn defaultOptions in
    button ([
        class_ "ui-icon",
        class' "selected" selected,
        class' "round" large,
        class' "hidden" hidden,
        disabled d
    ] <> props) $ [
        case icon of
            IconSymbol symbol -> svg ((uncurry style <$> st) <> [width "100%", height "100%"]) [
                use symbol [class_ "ui-icon-symbol"]
            ]
            IconText t -> span [class_ "ui-icon-text"] [text t]
            IconNone -> emptyNode,
        maybeN $ tooltip <#> \t -> span [class_ "ui-icon-tooltip"] [text t]
    ]
