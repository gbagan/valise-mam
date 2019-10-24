module UI.Template where
import Data.Maybe (Maybe(..))
import Optic.Core (Lens', (^.), (.~))
import Pha (VDom, text, emptyNode)
import Pha.Html (div', class')
import Lib.Game ((🎲), class Game, State, Dialog(..), _dialog, confirmNewGame)
import UI.Icons (winPanel)
import UI.Dialog (dialog)

type Elements a b = {
    board :: VDom b,
    config :: VDom b,
    rules :: Array (VDom b)
}

template :: forall a pos aux mov. Game pos aux mov => Lens' a (State pos aux) -> Elements (State pos aux) a -> State pos aux  -> VDom a
template lens elements state = 
    div' [] [
        div' [class' "main-container" true] [
            div' [] [elements.board, winPanel state],
            elements.config
        ],
    
        dialog' (state^._dialog)
    ]
    where
        dialog' Rules = 
            dialog {title: "Règles du jeu", onCancel: Nothing, onOk: Just (lens 🎲 _dialog .~ NoDialog)} elements.rules
        dialog' (ConfirmNewGame s) =
            dialog {title: "Nouvelle partie", onCancel: Just (lens 🎲 _dialog .~ NoDialog), onOk: Just (lens 🎲 confirmNewGame s)} [
                text "blah blah blah blah"
            ]
        dialog' _ = emptyNode