module UI.Dialog where
import Prelude
import Data.Maybe (Maybe)
import Data.Array (catMaybes)
import Pha.Action (Action)
import Pha (VDom, text)
import Pha.Html (div', button, class', click)

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
            div' [class' "ui-dialog-buttons" true] $ catMaybes [
                onCancel <#> \action ->
                    button [
                        class' "ui-button ui-button-primary" true, 
                        click action
                    ] [text "Annuler"],
                onOk <#> \action ->
                    button [
                        class' "ui-button ui-button-primary" true,
                        click action
                    ] [text "Ok"]
            ]
        ]
    ]

