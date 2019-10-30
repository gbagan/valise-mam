module UI.Template where
import Prelude
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Lens (Lens', (^.), (.~))
import Effect (Effect)
import Effect.Class (liftEffect)
import Pha.Class (VDom, Prop, Event, Action(..))
import Pha (text, emptyNode)
import Pha.Action (action, noAction, (🎲))
import Pha.Html (div', class', attr, style, pointerup, pointerdown, pointerleave, pointermove)
import Game.Core (class Game, State, Mode(..), PointerPosition, SizeLimit(..), Dialog(..),
         _dialog, _nbColumns, _nbRows, _customSize, _mode, _turn, _showWin, _pointerPosition, canPlay, sizeLimit,
         setGridSizeA, confirmNewGameA, dropA)
import UI.Dialog (dialog)
import UI.IncDecGrid (incDecGrid) as U

winPanel :: forall a b d. String -> State a b -> VDom d
winPanel title state =
    div' [class' "ui-flex-center ui-absolute component-win-container" true] [
        div' [class' "component-win" true, class' "visible" $ state^._showWin] [
            text title
        ]
    ]

card :: forall a. String -> Array (VDom a) -> VDom a
card title children =
    div' [class' "ui-card" true] [
        div' [class' "ui-card-head ui-flex-center" true] [
            div' [class' "ui-card-title" true] [text title]
        ],
        div' [class' "ui-card-body" true] children
    ]

gridStyle :: forall a. Int -> Int -> Int -> Array (Prop a)
gridStyle rows columns limit = [style "height" $ show (toNumber rows / m * 100.0) <> "%",
                                style "width" $ show (toNumber columns / m * 100.0) <> "%"]
    where m = toNumber $ max limit $ max rows columns

incDecGrid :: forall pos ext mov d. Game pos ext mov => Lens' d (State pos ext) -> State pos ext -> Array (VDom d) -> VDom d
incDecGrid lens state = U.incDecGrid {
    nbRows: state^._nbRows,
    nbColumns: state^._nbColumns,
    showRowButtons: minRows < maxRows,
    showColButtons: minCols < maxCols,
    customSize: state^._customSize,
    onResize: \x y -> lens 🎲 setGridSizeA x y true
} where
    SizeLimit minRows minCols maxRows maxCols = sizeLimit state 
    

type Elements a b = {
    board :: VDom b,
    config :: VDom b,
    rules :: Array (VDom b),
    winTitle :: String
}

template :: forall a pos aux mov. Game pos aux mov => Lens' a (State pos aux) -> Elements (State pos aux) a -> State pos aux  -> VDom a
template lens {board, config, rules, winTitle} state = 
    div' [] [
        div' [class' "main-container" true] [
            div' [] [board, winPanel winTitle state],
            config
        ],
        dialog' (state^._dialog)
    ]
    where
        dialog' Rules = 
            dialog {title: "Règles du jeu", onCancel: Nothing, onOk: Just $ lens 🎲 action (_dialog .~ NoDialog)} rules
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

cursorStyle :: forall a. PointerPosition -> Int -> Int -> Number -> Array (Prop a)    
cursorStyle {left, top} rows columns size = [
    style "left" $ show left <> "px",
    style "top" $ show top <> "px",
    style "width" $ show (size / toNumber columns) <> "%",
    style "height" $ show (size / toNumber rows) <> "%"
]

svgCursorStyle :: forall a. PointerPosition -> Array (Prop a)
svgCursorStyle {left, top, width, height} = [
    style "transform" $ "translate(" <> show (100.0 * left / width) <> "%," <> show (100.0 * top / height) <> "%"
]

trackPointer :: forall pos ext a. Lens' a (State pos ext) -> Array (Prop a)
trackPointer lens = [
    attr "touch-action" "none", 
    class' "ui-touch-action-none" true,
    pointermove $ lens 🎲 move,
    pointerleave $ lens 🎲  action (_pointerPosition .~ Nothing),
    pointerdown $ lens 🎲 move --  todo tester
] where
    move :: Action (State pos ext)
    move =  setPointerPositionA -- whenA  todo
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


dndBoardProps :: forall pos ext dnd a. Eq dnd => Game pos ext {from :: dnd, to :: dnd} =>
    Lens' a (State pos ext) -> Lens' (State pos ext) (Maybe dnd) -> Array (Prop a)
dndBoardProps lens dragLens = [
    attr "touch-action" "none", 
    class' "ui-touch-action-none" true,
    pointermove $ lens 🎲 move,
    pointerup $ lens 🎲 action (dragLens .~ Nothing), ---  (if droppable then "BOARD" else null),
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
    leave = action (_pointerPosition .~ Nothing)
            -- hasDnD && drop NoDrop

dndItemProps :: forall pos ext dnd a. Eq dnd => Game pos ext {from :: dnd, to :: dnd} =>
    Lens' a (State pos ext) -> Lens' (State pos ext) (Maybe dnd) -> Boolean -> Boolean -> dnd -> (State pos ext) -> Array (Prop a)
dndItemProps lens dragLens draggable droppable id state = [
    class' "dragged" dragged,
    class' "candrop" candrop,
    pointerdown $ if draggable then lens 🎲 action (dragLens .~ Just id) else noAction,  -- releasePointerCapture),
    pointerup $ lens 🎲 (if candrop then dropA dragLens id else action (dragLens .~ Nothing))  -- stopPropagation
] where
    draggedItem = state ^. dragLens
    candrop = droppable && (draggedItem # maybe false (\x -> canPlay state { from: x, to: id }))
    dragged = draggable && draggedItem == Just id


winTitleFor2Players :: forall pos ext. State pos ext -> String
winTitleFor2Players state =
    if state^._mode == DuelMode then
        "Le " <> (if state^._turn == 1 then "premier" else "second") <> " joueur gagne"
    else if state^._turn == 1 then
        "Tu as gagné"
    else
        "L\'IA gagne"
        