module UI.Template (template, incDecGrid) where
import Prelude
import Data.Maybe (Maybe(..))
import Optic.Core (Lens', (^.), (.~))
import Pha (VDom, text, emptyNode)
import Pha.Html (div', class')
import Lib.Game ((🎲), class Game, State, SizeLimit(..), Dialog(..),
        setCustomSize, _dialog, _nbColumns, _nbRows, _showWin, sizeLimit, confirmNewGame)
import UI.Dialog (dialog)
import UI.IncDecGrid (incDecGrid) as U

type Elements a b = {
    board :: VDom b,
    config :: VDom b,
    rules :: Array (VDom b)
}

winPanel :: forall a b d. State a b -> VDom d
winPanel state =
    div' [class' "ui-flex-center ui-absolute component-win-container" true] [
        div' [class' "component-win" true, class' "visible" $ state^._showWin] [
            text "GAGNÉ"
        ]
    ]

incDecGrid :: forall pos ext mov d. Game pos ext mov => Lens' d (State pos ext) -> State pos ext -> Array (VDom d) -> VDom d
incDecGrid lens state = U.incDecGrid {
    nbRows: state^._nbRows,
    nbColumns: state^._nbColumns,
    showRowButtons: minRows < maxRows,
    showColButtons: minCols < maxCols,
    customSize: true,
    onResize: \x y -> lens 🎲 (setCustomSize x y)
} where
    SizeLimit minRows minCols maxRows maxCols = sizeLimit state 
    

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