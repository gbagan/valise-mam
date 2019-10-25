module UI.Icon where

import Prelude
import Data.Maybe (Maybe (Nothing), maybe)
import Pha (text, VDom, Prop, emptyNode, h)
import Pha.Html (attr, div', span, svg, class', h2)

data Icon = IconText String | IconSymbol String | IconNone

type Options = {
    icon :: Icon,
    selected :: Boolean,
    tooltip :: Maybe String,
    round :: Boolean,
    large :: Boolean,
    hidden :: Boolean,
    disabled :: Boolean
}

defaultOptions :: Options
defaultOptions = {
    icon: IconNone,
    selected: false,
    tooltip: Nothing,
    round: false,
    large: false,
    hidden: false,
    disabled: false
}

iconbutton :: forall a. (Options -> Options) -> Array (Prop a) -> VDom a
iconbutton optionFn props =
    let {icon, selected, tooltip, round, large, hidden, disabled} = optionFn defaultOptions in
    div' ([
        class' "ui-icon" true,
        class' "selected" selected,
        class' "round" large,
        class' "hidden" hidden,
        class' "disabled" disabled
    ] <> props) [
        case icon of
            IconSymbol symbol ->
                svg [attr "width" "100%", attr "heigh" "100%", class' "ui-icon-symbol" true] [
                    h "use" [attr "href" symbol] []
                ]
            IconText t ->
                span [class' "ui-icon-text" true] [text t]
            IconNone ->
                emptyNode,  
        tooltip # maybe emptyNode (\t -> span [class' "ui-icon-tooltip" true] [text t])
    ]

icongroup :: forall a. String -> Array (VDom a) -> VDom a
icongroup title children =
    div' [] [
        h2 [] [text title],
        div' [class' "ui-icon-grid" true] children
    ]