module UI.Dialog where

import Data.Maybe (maybe)
import Pha (VDom, text, emptyNode)
import Pha.Html (div', span, button, class', click)

card :: forall a. String -> Array (VDom a) -> VDom a
card title children =
    div' [class' "ui-card" true] [
        div' [class' "ui-card-head ui-flex-center" true] [
            div' [class' "ui-card-title" true] [text title]
        ],
        div' [class' "ui-card-body" true] children
    ]

-- dialog :: forall a b. {title :: String } -> Array (VDom a b) -> VDom a b
-- dialog {title, onOk, onCancel} children =
--    div' [class' "ui-absolute ui-flex-center ui-dialog-container" true] [
--        div' [class' "ui-dialog" true] [
--            div' [class' "ui-dialog-head" true] [
--                div' [class' "ui-dialog-title"] [text title]
--            ],
--            div' [class' "ui-dialog-body" true], children,
--            div' [class' "ui-dialog-buttons" true] [
--                maybe emptyNode (\action ->
--                    button [
--                        class' "ui-button ui-button-primary",
--                        click action
--                    ]
--                ),
--                maybe emptyNode (\action ->
--                    button [
--                        class' "ui-button ui-button-primary" true, 
--                        click action
--                    ] [text "Ok"]
--                )
--            ]
--        ]
--    ]

