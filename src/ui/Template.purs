module UI.Template (template, gridStyle, incDecGrid) where
import Prelude
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Lens (Lens', (^.), (.~))
import Pha (VDom, Prop, text, emptyNode, (🎲))
import Pha.Html (div', class', style)
import Game.Core (class Game, State, SizeLimit(..), Dialog(..),
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

gridStyle :: forall a. Int -> Int -> Array (Prop a)
gridStyle rows columns = [style "height" $ show (toNumber rows / m * 100.0) <> "%",
                                style "width" $ show (toNumber columns / m * 100.0) <> "%"]
    where m = toNumber $ max 5 $ max rows columns

incDecGrid :: forall pos ext mov d. Game pos ext mov => Lens' d (State pos ext) -> State pos ext -> Array (VDom d) -> VDom d
incDecGrid lens state = U.incDecGrid {
    nbRows: state^._nbRows,
    nbColumns: state^._nbColumns,
    showRowButtons: minRows < maxRows,
    showColButtons: minCols < maxCols,
    customSize: true,
    onResize: \x y -> lens 🎲 setCustomSize x y
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
                text "Tu es sur le point de créer une nouvelle partie. Ta partie en cours sera perdue. Es-tu sûr(e)?"
            ]
        dialog' _ = emptyNode