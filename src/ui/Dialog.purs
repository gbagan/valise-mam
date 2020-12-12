module UI.Dialog where
import MyPrelude
import Pha as H
import Pha.Elements as HH
import Pha.Events as E

type DialogOptions a = {
    title ∷ String,
    onOk ∷ Maybe a,
    onCancel ∷ Maybe a
}

dialog ∷ ∀a. DialogOptions a → Array (H.VDom a) → H.VDom a
dialog {title, onOk, onCancel} children =
    HH.div [H.class_ "ui-absolute ui-flex-center ui-dialog-container"]
    [   HH.div [H.class_ "ui-dialog"]
        [   HH.div [H.class_ "ui-dialog-head"]
            [   HH.div [H.class_ "ui-dialog-title"] [H.text title]
            ]
        ,   HH.div [H.class_ "ui-dialog-body"] children
        ,   HH.div [H.class_ "ui-dialog-buttons"] 
            [   H.maybe onCancel \action →
                    HH.button 
                    [   H.class_ "ui-button ui-button-primary"
                    ,   E.onclick action
                    ]
                    [   H.text "Annuler"
                    ]
            ,   H.maybe onOk \action →
                    HH.button
                    [   H.class_ "ui-button ui-button-primary"
                    ,   E.onclick action
                    ]
                    [   H.text "Ok"
                    ]
            ]
        ]
    ]
