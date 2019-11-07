module UI.Template where
import MyPrelude
import Pha (VDom, Prop, text, emptyNode)
import Pha.Action (Action, action, (🔍))
import Pha.Html (div', class', attr, style, pointerup, pointerdown, pointerleave, pointermove)
import Game.Core (class Game, GState, Mode(..), SizeLimit(..), Dialog(..),
         _dialog, _nbColumns, _nbRows, _customSize, _mode, _turn, _showWin, _pointer, canPlay, isLevelFinished, sizeLimit,
         setGridSizeA, confirmNewGameA, dropA)
import UI.Dialog (dialog)
import UI.IncDecGrid (incDecGrid) as U
import Game.Effs (EFFS, getPointerPosition, releasePointerCapture, Position)

winPanel :: ∀a b d. String -> GState a b -> VDom d EFFS
winPanel title state =
    div' [class' "ui-flex-center ui-absolute component-win-container" true] [
        div' [class' "component-win" true, class' "visible" $ state^._showWin] [
            text title
        ]
    ]

card :: ∀a. String -> Array (VDom a EFFS) -> VDom a EFFS
card title children =
    div' [class' "ui-card" true] [
        div' [class' "ui-card-head ui-flex-center" true] [
            div' [class' "ui-card-title" true] [text title]
        ],
        div' [class' "ui-card-body" true] children
    ]

gridStyle :: ∀a. Int -> Int -> Int -> Array (Prop a EFFS)
gridStyle rows columns limit = [style "height" $ show (toNumber rows / m * 100.0) <> "%",
                                style "width" $ show (toNumber columns / m * 100.0) <> "%"]
    where m = toNumber $ max limit $ max rows columns

incDecGrid :: ∀pos ext mov d. Game pos ext mov => Lens' d (GState pos ext) -> GState pos ext -> Array (VDom d EFFS) 
                        -> VDom d EFFS
incDecGrid lens state = U.incDecGrid {
    nbRows: state^._nbRows,
    nbColumns: state^._nbColumns,
    showRowButtons: minRows < maxRows,
    showColButtons: minCols < maxCols,
    customSize: state^._customSize,
    onResize: \x y -> lens 🔍 setGridSizeA x y true
} where
    SizeLimit minRows minCols maxRows maxCols = sizeLimit state 
    

type Elements a = {
    board :: VDom a EFFS,
    config :: VDom a EFFS,
    rules :: Array (VDom a EFFS),
    winTitle :: String
}

template :: ∀a pos aux mov. Game pos aux mov =>
                Lens' a (GState pos aux) -> Elements a -> GState pos aux  -> VDom a EFFS
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
            dialog {title: "Règles du jeu", onCancel: Nothing, onOk: Just $ lens 🔍 action (_dialog .~ NoDialog)} rules
        dialog' (ConfirmNewGame s) =
            dialog {title: "Nouvelle partie", onCancel: Just $ lens 🔍 action (_dialog .~ NoDialog), onOk: Just (lens 🔍 confirmNewGameA s)} [
                text "Tu es sur le point de créer une nouvelle partie. Ta partie en cours sera perdue. Es-tu sûr(e)?"
            ]
        dialog' _ = emptyNode


setPointerPositionA :: ∀pos ext effs. (Maybe Position) -> Action (GState pos ext) effs
setPointerPositionA a = action $ _pointer .~ a

cursorStyle :: ∀a. Position -> Int -> Int -> Number -> Array (Prop a EFFS)    
cursorStyle {x, y} rows columns size = [
    style "left" $ show (x * 100.0) <> "%",
    style "top" $ show (y * 100.0) <> "%",
    style "width" $ show (size / toNumber columns) <> "%",
    style "height" $ show (size / toNumber rows) <> "%"
]

svgCursorStyle :: ∀a. Position -> Array (Prop a EFFS)
svgCursorStyle {x, y} = [
    style "transform" $ "translate(" <> show (100.0 * x) <> "%," <> show (100.0 * y) <> "%)"
]

trackPointer :: ∀pos ext a. Lens' a (GState pos ext) -> Array (Prop a EFFS)
trackPointer lens = [
    attr "touch-action" "none", 
    class' "ui-touch-action-none" true,
    pointermove $ lens 🔍 move,
    pointerleave $ lens 🔍  action (_pointer .~ Nothing),
    pointerdown $ lens 🔍 move
] where
    move = getPointerPosition >>= setPointerPositionA
        -- (\_ e -> pointerType e == Just "mouse")
        -- combine(
        --    whenA (\s -> s.pointer == Nothing) (actions.drop NoDrop)
        --)
    leave = -- combine(
           -- whenA
            --    (\_ e -> hasDnD || pointerType e == Just "mouse")
            action (_pointer .~ Nothing)

            -- hasDnD && drop NoDrop

dndBoardProps :: ∀pos ext dnd a. Eq dnd => Game pos ext {from :: dnd, to :: dnd} =>
    Lens' a (GState pos ext) -> Lens' (GState pos ext) (Maybe dnd) -> Array (Prop a EFFS)
dndBoardProps lens dragLens = [
    attr "touch-action" "none", 
    class' "ui-touch-action-none" true,
    pointermove $ lens 🔍 move,
    pointerup $ lens 🔍 action (dragLens .~ Nothing),
    pointerleave $ lens 🔍 leave,
    pointerdown $ lens 🔍 move
] where
    move = getPointerPosition >>= setPointerPositionA
        
        -- whenA
        -- (\_ e -> pointerType e == Just "mouse")
        -- combine(
        -- setPointerPosition -- `withPayload` relativePointerPosition
        --    whenA (\s -> s.pointer == Nothing) (actions.drop NoDrop)
        --)
    leave = action $ (_pointer .~ Nothing) ∘ (dragLens .~ Nothing)
            -- hasDnD && drop NoDrop

dndItemProps :: ∀pos ext dnd a. Eq dnd => Game pos ext {from :: dnd, to :: dnd} =>
    Lens' a (GState pos ext) -> Lens' (GState pos ext) (Maybe dnd) -> Boolean -> Boolean -> dnd -> (GState pos ext) -> Array (Prop a EFFS)
dndItemProps lens dragLens draggable droppable id state = [
    class' "dragged" dragged,
    class' "candrop" candrop,
    pointerdown $ if draggable then releasePointerCapture *> (lens 🔍 action (dragLens .~ Just id)) else pure unit,
    pointerup $ lens 🔍 (if candrop then dropA dragLens id else action (dragLens .~ Nothing))  -- stopPropagation
] where
    draggedItem = state ^. dragLens
    candrop = droppable && (draggedItem # maybe false (\x -> canPlay state { from: x, to: id }))
    dragged = draggable && draggedItem == Just id

turnMessage :: ∀pos ext mov. Game pos ext mov => GState pos ext -> String
turnMessage state =
    if isLevelFinished state then
        "Partie finie"
    else if state^._turn == 0 then
        "Tour du premier joueur"
    else if state^._mode == DuelMode then
        "Tour du second joueur"
    else 
        "Tour de l'IA"

winTitleFor2Players :: ∀pos ext. GState pos ext -> String
winTitleFor2Players state =
    if state^._mode == DuelMode then
        "Le " <> (if state^._turn == 1 then "premier" else "second") <> " joueur gagne"
    else if state^._turn == 1 then
        "Tu as gagné"
    else
        "L\'IA gagne"
        