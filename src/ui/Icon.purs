module UI.Icon where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Array (catMaybes)
import Data.Tuple (Tuple, uncurry)
import Pha (text)
import Pha.Class (VDom, Prop) 
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

iconbutton :: forall a. (Options -> Options) -> Array (Prop a) -> VDom a
iconbutton optionFn props =
    let {icon, selected, tooltip, round, large, hidden, disabled: d, style: st} = optionFn defaultOptions in
    button ([
        class' "ui-icon" true,
        class' "selected" selected,
        class' "round" large,
        class' "hidden" hidden,
        disabled d
    ] <> props) $ catMaybes [
        case icon of
            IconSymbol symbol -> Just $ svguse symbol $ [class' "ui-icon-symbol" true] <> (st <#> uncurry style) -- todo uncurry
            IconText t -> Just $ span [class' "ui-icon-text" true] [text t]
            IconNone -> Nothing,
        tooltip <#> \t -> span [class' "ui-icon-tooltip" true] [text t]
    ]
