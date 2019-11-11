module UI.Template where
import MyPrelude
import Pha (VDom, Prop, text, emptyNode)
import Pha.Action (Action, action, (🔍))
import Pha.Html (div', class', attr, style, translate, pc, pointerup, pointerdown, pointerleave, pointermove)
import Game.Core (class Game, GState, Mode(..), SizeLimit(..), Dialog(..),
         _dialog, _nbColumns, _nbRows, _customSize, _mode, _turn, _showWin, _pointer, _locked, 
         canPlay, isLevelFinished, sizeLimit, setGridSizeA, confirmNewGameA, dropA)
import UI.Dialog (dialog) as D
import UI.IncDecGrid (incDecGrid) as U
import Game.Effs (EFFS, getPointerPosition, releasePointerCapture, Position)

winPanel :: ∀a effs. String -> Boolean -> VDom a effs
winPanel title visible =
    div' [class' "ui-flex-center ui-absolute component-win-container" true] [
        div' [class' "component-win" true, class' "visible" visible] [
            text title
        ]
    ]

card :: ∀a effs. String -> Array (VDom a effs) -> VDom a effs
card title children =
    div' [class' "ui-card" true] [
        div' [class' "ui-card-head ui-flex-center" true] [
            div' [class' "ui-card-title" true] [text title]
        ],
        div' [class' "ui-card-body" true] children
    ]

incDecGrid :: ∀pos ext mov d. Game pos ext mov => Lens' d (GState pos ext) -> GState pos ext -> Array (VDom d EFFS) 
                        -> VDom d EFFS
incDecGrid lens state = U.incDecGrid {
    locked: state^._locked,
    nbRows: state^._nbRows,
    nbColumns: state^._nbColumns,
    showRowButtons: minRows < maxRows,
    showColButtons: minCols < maxCols,
    customSize: state^._customSize,
    onResize: \x y -> lens 🔍 setGridSizeA x y true
} where
    SizeLimit minRows minCols maxRows maxCols = sizeLimit state 

type Elements a effs = {
    board :: VDom a effs,
    config :: VDom a effs,
    rules :: Array (VDom a effs),
    winTitle :: String,
    customDialog :: Unit -> VDom a effs,
    scoreDialog :: Unit -> VDom a effs
}

defaultElements :: ∀a effs. Elements a effs
defaultElements = {
    board: emptyNode,
    config: emptyNode,
    rules: [text "blah blah"],
    winTitle: "GAGNÉ",
    customDialog: \_ -> emptyNode,
    scoreDialog: \_ -> emptyNode
}

dialog :: ∀a pos aux effs. Lens' a (GState pos aux) -> String -> Array (VDom a effs) -> VDom a effs
dialog lens title = D.dialog {title, onCancel: Nothing, onOk: Just $ lens 🔍 action (_dialog .~ NoDialog)}

template :: ∀a pos aux mov. Game pos aux mov =>
                Lens' a (GState pos aux) -> (Elements a EFFS -> Elements a EFFS) -> GState pos aux  -> VDom a EFFS
template lens elemFn state =
    div' [] [
        div' [class' "main-container" true] [
            div' [] [board, winPanel winTitle (state^._showWin)],
            config
        ],
        dialog' (state^._dialog)
    ]
    where
        {board, config, rules, winTitle, customDialog, scoreDialog} = elemFn defaultElements
        dialog' Rules = dialog lens "Règles du jeu" rules
        dialog' (ConfirmNewGame s) =
            D.dialog {title: "Nouvelle partie", onCancel: Just $ lens 🔍 action (_dialog .~ NoDialog), onOk: Just (lens 🔍 confirmNewGameA s)} [
                text "Tu es sur le point de créer une nouvelle partie. Ta partie en cours sera perdue. Es-tu sûr(e)?"
            ]
        dialog' CustomDialog = customDialog unit
        dialog' ScoreDialog = scoreDialog unit
        dialog' _ = emptyNode


gridStyle :: ∀a effs. Int -> Int -> Int -> Array (Prop a effs)
gridStyle rows columns limit = [style "height" $ pc (toNumber rows / m * 100.0),
                                style "width" $ pc (toNumber columns / m * 100.0)]
    where m = toNumber $ max limit $ max rows columns        

setPointerPositionA :: ∀pos ext effs. (Maybe Position) -> Action (GState pos ext) effs
setPointerPositionA a = action $ _pointer .~ a

cursorStyle :: ∀a effs. Position -> Int -> Int -> Number -> Array (Prop a effs)    
cursorStyle {x, y} rows columns size = [
    style "left" $ pc (x * 100.0),
    style "top" $ pc (y * 100.0),
    style "width" $ pc (size / toNumber columns),
    style "height" $ pc (size / toNumber rows)
]

svgCursorStyle :: ∀a effs. Position -> Array (Prop a effs)
svgCursorStyle {x, y} = [
    style "transform" $ translate (pc $ 100.0 * x) (pc $ 100.0 * y)
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
        "L'IA gagne"
        