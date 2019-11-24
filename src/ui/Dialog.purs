module UI.Dialog where
import MyPrelude
import Pha.Action (Action)
import Pha (VDom, text, class_)
import Pha.Elements (div, button)
import Pha.Attributes (onclick)

type DialogOptions a effs = {
    title :: String,
    onOk :: Maybe (Action a effs),
    onCancel :: Maybe (Action a effs)
}

dialog :: ∀a effs. DialogOptions a effs -> Array (VDom a effs) -> VDom a effs
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
