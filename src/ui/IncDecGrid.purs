module UI.IncDecGrid where 

import MyPrelude
import Pha as H
import Pha.Elements as HH
import Pha.Events as E
import UI.Icon (iconbutton, Icon(..))

type IncDecGridOptions msg =
    {   locked ∷ Boolean
    ,   nbRows ∷ Int
    ,   nbColumns ∷ Int
    ,   customSize ∷ Boolean
    ,   showRowButtons ∷ Boolean
    ,   showColButtons ∷ Boolean
    ,   resize ∷ Int → Int → msg
    }

incDecGrid ∷ ∀msg. IncDecGridOptions msg → Array (H.VDom msg) → H.VDom msg   
incDecGrid {locked, nbRows, nbColumns, customSize, showRowButtons, showColButtons, resize} children =
    HH.div [H.class_ "ui-incdecgrid"]
    [   HH.div [H.class_ "flex"]
        [   HH.div [H.class_ "ui-flex-center ui-incdecgrid-container"] children
        ,   H.when showRowButtons \_ →
                HH.div [H.class_ "ui-flex-center ui-incdecgrid-rows"]
                [   iconbutton
                    {   round: true
                    ,   icon: IconSymbol "#plus"
                    ,   disabled: locked
                    ,   hidden: not customSize
                    }
                    [E.onclick $ resize (nbRows + 1) nbColumns]
                ,   HH.div [H.class_ "ui-incdecgrid-text"] [ HH.span [] [ H.text $ show nbRows ] ]
                ,   iconbutton
                    {   round: true
                    ,   icon: IconSymbol "#minus"
                    ,   disabled: locked
                    ,   hidden: not customSize
                    }
                    [E.onclick $ resize (nbRows - 1) nbColumns]
                ]
        ]
    ,   H.when showColButtons \_ →
            HH.div [H.class_ "ui-flex-center ui-incdecgrid-cols"]
            [   iconbutton
                {   round: true
                ,   icon: IconSymbol "#minus"
                ,   disabled: locked
                ,   hidden: not customSize
                }
                [E.onclick $ resize nbRows (nbColumns - 1)]
            ,   HH.div [H.class_ "ui-incdecgrid-text"] [ HH.span [] [ H.text $ show nbColumns ] ]
            ,   iconbutton
                {   round: true
                ,   icon: IconSymbol "#plus"
                ,   disabled: locked, hidden: not customSize
                }
                [E.onclick $ resize nbRows (nbColumns + 1)]
            ]
    ]
