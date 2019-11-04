module UI.Template where
import MyPrelude
import Effect (Effect)
import Pha (VDom, Prop, text, emptyNode)
import Pha.Action (Event, Action, action, withPayload', onlyEffectAction, (🎲))
import Pha.Html (div', class', attr, style, pointerup, pointerdown, pointerleave, pointermove)
import Game.Core (class Game, GState, Mode(..), PointerPosition, SizeLimit(..), Dialog(..),
         _dialog, _nbColumns, _nbRows, _customSize, _mode, _turn, _showWin, _pointer, canPlay, isLevelFinished, sizeLimit,
         setGridSizeA, confirmNewGameA, dropA)
import UI.Dialog (dialog)
import UI.IncDecGrid (incDecGrid) as U

winPanel :: forall a b d. String -> GState a b -> VDom d
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

incDecGrid :: forall pos ext mov d. Game pos ext mov => Lens' d (GState pos ext) -> GState pos ext -> Array (VDom d) -> VDom d
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

template :: forall a pos aux mov. Game pos aux mov =>
                Lens' a (GState pos aux) -> Elements (GState pos aux) a -> GState pos aux  -> VDom a
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

-- todo
foreign import relativePointerPositionAux :: Maybe PointerPosition -> (PointerPosition -> Maybe PointerPosition) 
                                            -> Event -> Effect (Maybe PointerPosition)

relativePointerPosition :: Event -> Effect (Maybe PointerPosition)
relativePointerPosition = relativePointerPositionAux Nothing Just

foreign import releasePointerCapture :: Event -> Effect Unit

releasePointerCaptureA :: forall a. Action a
releasePointerCaptureA = onlyEffectAction releasePointerCapture

setPointerPositionA :: forall pos ext. (Maybe PointerPosition) -> Action (GState pos ext)
setPointerPositionA a = action $ _pointer .~ a

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

trackPointer :: forall pos ext a. Lens' a (GState pos ext) -> Array (Prop a)
trackPointer lens = [
    attr "touch-action" "none", 
    class' "ui-touch-action-none" true,
    pointermove $ lens 🎲 move,
    pointerleave $ lens 🎲  action (_pointer .~ Nothing),
    pointerdown $ lens 🎲 move --  todo tester
] where
    move :: Action (GState pos ext)
    move =  setPointerPositionA  `withPayload'` relativePointerPosition
        -- (\_ e -> pointerType e == Just "mouse")
        -- combine(
        --    whenA (\s -> s.pointer == Nothing) (actions.drop NoDrop)
        --)
    leave = -- combine(
           -- whenA
            --    (\_ e -> hasDnD || pointerType e == Just "mouse")
            action (_pointer .~ Nothing)

            -- hasDnD && drop NoDrop


dndBoardProps :: forall pos ext dnd a. Eq dnd => Game pos ext {from :: dnd, to :: dnd} =>
    Lens' a (GState pos ext) -> Lens' (GState pos ext) (Maybe dnd) -> Array (Prop a)
dndBoardProps lens dragLens = [
    attr "touch-action" "none", 
    class' "ui-touch-action-none" true,
    pointermove $ lens 🎲 move,
    pointerup $ lens 🎲 action (dragLens .~ Nothing),
    pointerleave $ lens 🎲 leave,
    pointerdown $ lens 🎲 move --  todo tester
] where
    move :: Action (GState pos ext)
    move = setPointerPositionA `withPayload'` relativePointerPosition
        
        -- whenA
        -- (\_ e -> pointerType e == Just "mouse")
        -- combine(
        -- setPointerPosition -- `withPayload` relativePointerPosition
        --    whenA (\s -> s.pointer == Nothing) (actions.drop NoDrop)
        --)
    leave = action $ (_pointer .~ Nothing) ∘ (dragLens .~ Nothing)
            -- hasDnD && drop NoDrop

dndItemProps :: forall pos ext dnd a. Eq dnd => Game pos ext {from :: dnd, to :: dnd} =>
    Lens' a (GState pos ext) -> Lens' (GState pos ext) (Maybe dnd) -> Boolean -> Boolean -> dnd -> (GState pos ext) -> Array (Prop a)
dndItemProps lens dragLens draggable droppable id state = [
    class' "dragged" dragged,
    class' "candrop" candrop,
    pointerdown $ if draggable then lens 🎲 action (dragLens .~ Just id) <> releasePointerCaptureA else mempty,
    pointerup $ lens 🎲 (if candrop then dropA dragLens id else action (dragLens .~ Nothing))  -- stopPropagation
] where
    draggedItem = state ^. dragLens
    candrop = droppable && (draggedItem # maybe false (\x -> canPlay state { from: x, to: id }))
    dragged = draggable && draggedItem == Just id

turnMessage :: forall pos ext mov. Game pos ext mov => GState pos ext -> String
turnMessage state =
    if isLevelFinished state then
        "Partie finie"
    else if state^._turn == 0 then
        "Tour du premier joueur"
    else if state^._mode == DuelMode then
        "Tour du second joueur"
    else 
        "Tour de l'IA"

winTitleFor2Players :: forall pos ext. GState pos ext -> String
winTitleFor2Players state =
    if state^._mode == DuelMode then
        "Le " <> (if state^._turn == 1 then "premier" else "second") <> " joueur gagne"
    else if state^._turn == 1 then
        "Tu as gagné"
    else
        "L\'IA gagne"
        