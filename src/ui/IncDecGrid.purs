module UI.IncDecGrid where 

import Prelude
import Pha (VDom, text, whenN)
import Pha.Action (Action)
import Pha.Html (div', span, class', click)
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
    div' [class' "ui-incdecgrid" true] [
        div' [class' "flex" true] [
            div' [class' "ui-flex-center ui-incdecgrid-container" true] children,
            whenN showRowButtons \_ ->
                div' [class' "ui-flex-center ui-incdecgrid-rows" true] [
                    iconbutton
                        ((_{round = true, icon = IconSymbol "#plus", disabled = locked, hidden = not customSize}))
                        [click $ onResize (nbRows + 1) nbColumns],
                    div' [class' "ui-incdecgrid-text" true] [ span [] [ text $ show nbRows ] ],
                    iconbutton
                        ((_{round = true, icon = IconSymbol "#minus", disabled = locked, hidden = not customSize}))
                        [click $ onResize (nbRows - 1) nbColumns]
                ]
        ],
        whenN showColButtons \_ ->
            div' [class' "ui-flex-center ui-incdecgrid-cols" true] [ 
                iconbutton
                    (\x -> x{round = true, icon = IconSymbol "#minus", disabled = locked, hidden = not customSize})
                    [click $ onResize nbRows (nbColumns - 1)],
                div' [class' "ui-incdecgrid-text" true] [ span [] [ text $ show nbColumns ] ],
                iconbutton
                    (\x -> x{round = true, icon = IconSymbol "#plus", disabled = locked, hidden = not customSize})
                    [click $ onResize nbRows (nbColumns + 1)]
            ]
    ]
    
    
