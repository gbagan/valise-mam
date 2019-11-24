module UI.IncDecGrid where 

import MyPrelude
import Pha (VDom, text, ifN, class_, class')
import Pha.Action (Action)
import Pha.Elements (div, span)
import Pha.Attributes (onclick)
import UI.Icon (iconbutton, Icon(..))

type IncDecGridOptions a effs = {
    locked :: Boolean,
    nbRows :: Int,
    nbColumns :: Int,
    customSize :: Boolean,
    showRowButtons :: Boolean,
    showColButtons :: Boolean,
    onResize :: Int -> Int -> Action a effs
}

incDecGrid :: ∀a effs. IncDecGridOptions a effs -> Array (VDom a effs) -> VDom a effs   
incDecGrid {locked, nbRows, nbColumns, customSize, showRowButtons, showColButtons, onResize} children =
    div [class_ "ui-incdecgrid"] [
        div [class_ "flex"] [
            div [class_ "ui-flex-center ui-incdecgrid-container"] children,
            ifN showRowButtons \_ ->
                div [class_ "ui-flex-center ui-incdecgrid-rows"] [
                    iconbutton
                        _{round = true, icon = IconSymbol "#plus", disabled = locked, hidden = not customSize}
                        [onclick $ onResize (nbRows + 1) nbColumns],
                    div [class_ "ui-incdecgrid-text"] [ span [] [ text $ show nbRows ] ],
                    iconbutton
                        _{round = true, icon = IconSymbol "#minus", disabled = locked, hidden = not customSize}
                        [onclick $ onResize (nbRows - 1) nbColumns]
                ]
        ],
        ifN showColButtons \_ ->
            div [class_ "ui-flex-center ui-incdecgrid-cols"] [ 
                iconbutton
                    _{round = true, icon = IconSymbol "#minus", disabled = locked, hidden = not customSize}
                    [onclick $ onResize nbRows (nbColumns - 1)],
                div [class_ "ui-incdecgrid-text"] [ span [] [ text $ show nbColumns ] ],
                iconbutton
                    _{round = true, icon = IconSymbol "#plus", disabled = locked, hidden = not customSize}
                    [onclick $ onResize nbRows (nbColumns + 1)]
            ]
    ]
