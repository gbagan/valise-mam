module UI.Dialog where
import MyPrelude
import Pha.Action (Action)
import Pha (VDom, text, class_)
import Pha.Elements (div, button)
import Pha.Events (onclick)

type DialogOptions a = {
    title :: String,
    onOk :: Maybe a,
    onCancel :: Maybe a
}

dialog :: ∀a effs. DialogOptions a -> Array (VDom a) -> VDom a
dialog {title, onOk, onCancel} children =
    div [class_ "ui-absolute ui-flex-center ui-dialog-container"] [
        div [class_ "ui-dialog"] [
            div [class_ "ui-dialog-head"] [
                div [class_ "ui-dialog-title"] [text title]
            ],
            div [class_ "ui-dialog-body"] children,
            div [class_ "ui-dialog-buttons"] $ catMaybes [
                onCancel <#> \action ->
                    button [
                        class_ "ui-button ui-button-primary", 
                        onclick action
                    ] [text "Annuler"],
                onOk <#> \action ->
                    button [
                        class_ "ui-button ui-button-primary",
                        onclick action
                    ] [text "Ok"]
            ]
        ]
    ]
