module UI.Template where
import Prelude
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Lens (Lens', (^.), (.~))
import Effect (Effect)
import Effect.Class (liftEffect)
import Pha (VDom, Prop, Event, Action(..), action, text,  emptyNode, (🎲), whenA)
import Pha.Html (div', class', attr, style, pointerup, pointerdown, pointerleave, pointermove)
import Pha.Event (pointerType)
import Game.Core (class Game, State, PointerPosition, SizeLimit(..), Dialog(..),
         _dialog, _nbColumns, _nbRows, _showWin, _pointerPosition, sizeLimit,
         setCustomSizeA, confirmNewGameA)
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
    onResize: \x y -> lens 🎲 setCustomSizeA x y
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
            dialog {title: "Règles du jeu", onCancel: Nothing, onOk: Just $ lens 🎲 action (_dialog .~ NoDialog)} elements.rules
        dialog' (ConfirmNewGame s) =
            dialog {title: "Nouvelle partie", onCancel: Just $ lens 🎲 action (_dialog .~ NoDialog), onOk: Just (lens 🎲 confirmNewGameA s)} [
                text "Tu es sur le point de créer une nouvelle partie. Ta partie en cours sera perdue. Es-tu sûr(e)?"
            ]
        dialog' _ = emptyNode


foreign import relativePointerPositionAux :: Maybe PointerPosition -> (PointerPosition -> Maybe PointerPosition) -> Event -> Effect (Maybe PointerPosition)

relativePointerPosition :: Event -> Effect (Maybe PointerPosition)
relativePointerPosition = relativePointerPositionAux Nothing Just

setPointerPositionA :: forall pos ext. Action (State pos ext)
setPointerPositionA = Action \setState ev state -> liftEffect $ do
    pos <- relativePointerPosition ev
    setState $ state # _pointerPosition .~ pos

svgCursorStyle :: forall a. PointerPosition -> Array (Prop a)
svgCursorStyle {left, top, width, height} = [
    style "transform" $ "translate(" <> show (100.0 * left / width) <> "%," <> show (100.0 * top / height) <> "%"
]


trackPointer :: forall a drag pos ext. Lens' a (State pos ext) -> Array (Prop a)
trackPointer lens = [
    attr "touch-action" "none", 
    class' "ui-touch-action-none" true,
    pointermove $ lens 🎲 move,
    -- pointerup $ lens 🎲 whenA hasDnD && drop Nothing  ---  (if droppable then "BOARD" else null),
    pointerleave $ lens 🎲 leave,
    pointerdown $ lens 🎲 move --  todo tester
] where
    move :: Action (State pos ext)
    move =  setPointerPositionA -- whenA
        -- (\_ e -> pointerType e == Just "mouse")
        -- combine(
        -- setPointerPosition -- `withPayload` relativePointerPosition
        --    whenA (\s -> s.pointerPosition == Nothing) (actions.drop NoDrop)
        --)
    leave = -- combine(
           -- whenA
            --    (\_ e -> hasDnD || pointerType e == Just "mouse")
            action (_pointerPosition .~ Nothing)

            -- hasDnD && drop NoDrop
{-
            Item: ({ tag = 'div', draggable, droppable, id, class: class2, drop, ...attrs }, children) => {
                const candrop = droppable && state.dragged !== null && O.canPlay(state, { from: state.dragged, to: id });
                const dragged = draggable && id === state.dragged; 
                const dropHandler = candrop && combine([drop || actions.drop, id], stopPropagation);
                return h(tag, {
                    class: {
                        ...toObject(class2),
                        dragged,
                        candrop
                    },
                    onpointerdown: draggable && combine([actions.drag, id], releasePointerCapture),
                    onpointerup: dropHandler,
                    ...attrs
                }, children);
            }
-}