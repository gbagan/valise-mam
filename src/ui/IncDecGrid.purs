module UI.IncDecGrid where 

import MyPrelude
import Pha (VDom, text, (<&&>), class_)
import Pha.Elements (div, span)
import Pha.Events (onclick)
import UI.Icon (iconbutton, Icon(..))

type IncDecGridOptions msg = {
    locked ∷ Boolean,
    nbRows ∷ Int,
    nbColumns ∷ Int,
    customSize ∷ Boolean,
    showRowButtons ∷ Boolean,
    showColButtons ∷ Boolean,
    resize ∷ Int → Int → msg
}

incDecGrid ∷ ∀msg. IncDecGridOptions msg → Array (VDom msg) → VDom msg   
incDecGrid {locked, nbRows, nbColumns, customSize, showRowButtons, showColButtons, resize} children =
    div [class_ "ui-incdecgrid"] [
        div [class_ "flex"] [
            div [class_ "ui-flex-center ui-incdecgrid-container"] children,
            showRowButtons <&&> \_ →
                div [class_ "ui-flex-center ui-incdecgrid-rows"] [
                    iconbutton
                        {round: true, icon: IconSymbol "#plus", disabled: locked, hidden: not customSize}
                        [onclick $ resize (nbRows + 1) nbColumns],
                    div [class_ "ui-incdecgrid-text"] [ span [] [ text $ show nbRows ] ],
                    iconbutton
                        {round: true, icon: IconSymbol "#minus", disabled: locked, hidden: not customSize}
                        [onclick $ resize (nbRows - 1) nbColumns]
                ]
        ],
        showColButtons <&&> \_ →
            div [class_ "ui-flex-center ui-incdecgrid-cols"] [ 
                iconbutton
                    {round: true, icon: IconSymbol "#minus", disabled: locked, hidden: not customSize}
                    [onclick $ resize nbRows (nbColumns - 1)],
                div [class_ "ui-incdecgrid-text"] [ span [] [ text $ show nbColumns ] ],
                iconbutton
                    {round: true, icon: IconSymbol "#plus", disabled: locked, hidden: not customSize}
                    [onclick $ resize nbRows (nbColumns + 1)]
            ]
    ]
