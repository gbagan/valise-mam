module UI.IncDecGrid where 

import Prelude
import Pha (VDom, text, whenN)
import Pha.Action (Action)
import Pha.Html (div', span, class', click)
import UI.Icon (iconbutton, Icon(..))

type IncDecGridOptions a = {
    nbRows :: Int,
    nbColumns :: Int,
    customSize :: Boolean,
    showRowButtons :: Boolean,
    showColButtons :: Boolean,
    onResize :: Int -> Int -> Action a
}

incDecGrid :: forall a. IncDecGridOptions a -> Array (VDom a) -> VDom a   
incDecGrid {nbRows, nbColumns, customSize, showRowButtons, showColButtons, onResize} children =
    div' [class' "ui-incdecgrid" true] [
        div' [class' "flex" true] [
            div' [class' "ui-flex-center ui-incdecgrid-container" true] children,
            whenN showRowButtons \_ ->
                div' [class' "ui-flex-center ui-incdecgrid-rows" true] [
                    iconbutton
                        ((_{round = true, icon = IconSymbol "#plus", hidden = not customSize}))
                        [click $ onResize (nbRows + 1) nbColumns],
                    div' [class' "ui-incdecgrid-text" true] [ span [] [ text $ show nbRows ] ],
                    iconbutton
                        ((_{round = true, icon = IconSymbol "#minus", hidden = not customSize}))
                        [click $ onResize (nbRows - 1) nbColumns]
                ]
        ],
        whenN showColButtons \_ ->
            div' [class' "ui-flex-center ui-incdecgrid-cols" true] [ 
                iconbutton
                    (\x -> x{round = true, icon = IconSymbol "#minus", hidden = not customSize})
                    [click $ onResize nbRows (nbColumns - 1)],
                div' [class' "ui-incdecgrid-text" true] [ span [] [ text $ show nbColumns ] ],
                iconbutton
                    (\x -> x{round = true, icon = IconSymbol "#plus", hidden = not customSize})
                    [click $ onResize nbRows (nbColumns + 1)]
            ]
    ]
    
    
