module UI.Dialog where
import MyPrelude
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Events as E

type DialogOptions a = {
    title ∷ String,
    onOk ∷ Maybe a,
    onCancel ∷ Maybe a
}

dialog ∷ ∀a. DialogOptions a → Array (Html a) → Html a
dialog {title, onOk, onCancel} children =
    H.div [H.class_ "ui-absolute ui-flex-center ui-dialog-container"]
    [   H.div [H.class_ "ui-dialog"]
        [   H.div [H.class_ "ui-dialog-head"]
            [   H.div [H.class_ "ui-dialog-title"] [H.text title]
            ]
        ,   H.div [H.class_ "ui-dialog-body"] children
        ,   H.div [H.class_ "ui-dialog-buttons"] 
            [   H.maybe onCancel \action →
                    H.button 
                    [   H.class_ "ui-button ui-button-primary"
                    ,   E.onClick action
                    ]
                    [   H.text "Annuler"
                    ]
            ,   H.maybe onOk \action →
                    H.button
                    [   H.class_ "ui-button ui-button-primary"
                    ,   E.onClick action
                    ]
                    [   H.text "Ok"
                    ]
            ]
        ]
    ]
