module UI.Dialog where
import Prelude
import Data.Maybe (Maybe, maybe)
import Pha.Class (VDom, Action) 
import Pha (text, emptyNode)
import Pha.Html (div', button, class', click)

card :: forall a. String -> Array (VDom a) -> VDom a
card title children =
    div' [class' "ui-card" true] [
        div' [class' "ui-card-head ui-flex-center" true] [
            div' [class' "ui-card-title" true] [text title]
        ],
        div' [class' "ui-card-body" true] children
    ]

type DialogOptions a = {
    title :: String,
    onOk :: Maybe (Action a),
    onCancel :: Maybe (Action a)
}

dialog :: forall a. DialogOptions a -> Array (VDom a) -> VDom a
dialog {title, onOk, onCancel} children =
    div' [class' "ui-absolute ui-flex-center ui-dialog-container" true] [
        div' [class' "ui-dialog" true] [
            div' [class' "ui-dialog-head" true] [
                div' [class' "ui-dialog-title" true] [text title]
            ],
            div' [class' "ui-dialog-body" true] children,
            div' [class' "ui-dialog-buttons" true] [
                onCancel # maybe emptyNode (\action ->
                    button [
                        class' "ui-button ui-button-primary" true, 
                        click action
                    ] [text "Annuler"]
                ),
                onOk # maybe emptyNode (\action ->
                    button [
                        class' "ui-button ui-button-primary" true,
                        click action
                    ] [text "Ok"]
                )
            ]
        ]
    ]

