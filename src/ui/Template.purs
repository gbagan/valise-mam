module UI.Template where

import Optic.Core (Lens', (^.))
import Pha (VDom)
import Pha.Html (div', class')
import Lib.Game (class Game, State, Dialog, _dialog)
import UI.Icons (winPanel)

type Elements a b = {
    board :: VDom b,
    config :: VDom b,
    dialog :: Dialog a -> VDom b
}

template :: forall a pos aux mov. Game pos aux mov => Lens' a (State pos aux) -> Elements (State pos aux) a -> State pos aux  -> VDom a
template lens elements state = 
    div' [] [
        div' [class' "main-container" true] [
            div' [] [elements.board, winPanel state],
            elements.config
        ],
    
        elements.dialog (state^._dialog)
    ]