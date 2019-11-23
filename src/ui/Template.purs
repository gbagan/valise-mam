module UI.Template where
import MyPrelude
import Pha (VDom, Prop, text, emptyNode, maybeN)
import Pha.Action (Action, setState)
import Pha.Html (div', class', style, pc, onpointerup, onpointerdown', onpointerleave, onpointermove')
import Pha.Util (translate)
import Game.Core (class Game, class ScoreGame, GState, Mode(..), Turn(..), SizeLimit(..), Dialog(..),
         _dialog, _nbColumns, _nbRows, _customSize, _mode, _turn, _showWin, _pointer, _locked, 
         canPlay, isLevelFinished, sizeLimit, bestScore, setGridSizeA, confirmNewGameA, dropA)
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

incDecGrid :: ∀pos ext mov. Game pos ext mov => GState pos ext -> Array (VDom (GState pos ext) EFFS) -> VDom (GState pos ext) EFFS
incDecGrid state = U.incDecGrid {
    locked: state^._locked,
    nbRows: state^._nbRows,
    nbColumns: state^._nbColumns,
    showRowButtons: minRows < maxRows,
    showColButtons: minCols < maxCols,
    customSize: state^._customSize,
    onResize: \x y -> setGridSizeA x y true
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

dialog :: ∀pos ext effs. String -> Array (VDom (GState pos ext) effs) -> VDom (GState pos ext) effs
dialog title = D.dialog {title, onCancel: Nothing, onOk: Just $ setState (_dialog .~ NoDialog)}

bestScoreDialog :: ∀pos ext mov effs. ScoreGame pos ext mov => GState pos ext
                                  -> (pos -> Array (VDom (GState pos ext) effs)) -> VDom (GState pos ext) effs
bestScoreDialog state children = maybeN $ bestScore state <#> snd <#> \pos ->
    dialog "Meilleur score" (children pos)

template :: ∀pos ext mov. Game pos ext mov =>
            (Elements (GState pos ext) EFFS -> Elements (GState pos ext) EFFS) 
            -> GState pos ext -> VDom (GState pos ext) EFFS
template elemFn state =
    div' [] [
        div' [class' "main-container" true] [
            div' [] [board, winPanel winTitle (state^._showWin)],
            config
        ],
        dialog' (state^._dialog)
    ]
    where
        {board, config, rules, winTitle, customDialog, scoreDialog} = elemFn defaultElements
        dialog' Rules = dialog "Règles du jeu" rules
        dialog' (ConfirmNewGame s) =
            D.dialog {
                title: "Nouvelle partie", 
                onCancel: Just $ setState (_dialog .~ NoDialog), 
                onOk: Just $ confirmNewGameA s
            } [
                text "Tu es sur le point de créer une nouvelle partie. Ta partie en cours sera perdue. Es-tu sûr(e)?"
            ]
        dialog' CustomDialog = customDialog unit
        dialog' ScoreDialog = scoreDialog unit
        dialog' _ = emptyNode


gridStyle :: ∀a effs. Int -> Int -> Int -> Array (Prop a effs)
gridStyle rows columns limit = [style "height" $ pc (toNumber rows / m),
                                style "width" $ pc (toNumber columns / m)]
    where m = toNumber $ max limit $ max rows columns        

setPointerPositionA :: ∀pos ext effs. (Maybe Position) -> Action (GState pos ext) effs
setPointerPositionA a = setState (_pointer .~ a)

cursorStyle :: ∀a effs. Position -> Int -> Int -> Number -> Array (Prop a effs)    
cursorStyle {x, y} rows columns size = [
    style "left" $ pc x,
    style "top" $ pc y,
    style "width" $ pc (size / toNumber columns),
    style "height" $ pc (size / toNumber rows)
]

svgCursorStyle :: ∀a effs. Position -> Array (Prop a effs)
svgCursorStyle {x, y} = [
    style "transform" $ translate (pc x) (pc y)
]

-- style à appliquer sur l'élément DOM représentant le plateau
-- permet de mémoriser la position du pointeur
trackPointer :: ∀pos ext. Array (Prop (GState pos ext) EFFS)
trackPointer = [
    style "touch-action" "none",
    onpointermove' move,
    onpointerleave $ setState (_pointer .~ Nothing),
    onpointerdown' move
] where
    move ev = getPointerPosition ev >>= setPointerPositionA

-- même chose que trackPointer mais gère le drag and drop par l'intermédiaire d'un lens
dndBoardProps :: ∀pos ext dnd. Eq dnd => Game pos ext {from :: dnd, to :: dnd} =>
    Lens' (GState pos ext) (Maybe dnd) -> Array (Prop (GState pos ext) EFFS)
dndBoardProps dragLens = [
    style "touch-action" "none", 
    onpointermove' move,
    onpointerup $ setState (dragLens .~ Nothing),
    onpointerleave leave,
    onpointerdown' move
] where
    move ev = getPointerPosition ev >>= setPointerPositionA
    leave = setState $ (_pointer .~ Nothing) ∘ (dragLens .~ Nothing)

dndItemProps :: ∀pos ext dnd. Eq dnd => Game pos ext {from :: dnd, to :: dnd} =>
    Lens' (GState pos ext) (Maybe dnd) -> Boolean -> Boolean -> dnd -> (GState pos ext) -> Array (Prop (GState pos ext) EFFS)
dndItemProps dragLens draggable droppable id state = [
    class' "dragged" dragged,
    class' "candrop" candrop,
    onpointerdown' $ \ev -> when draggable $ releasePointerCapture ev *> setState (dragLens .~ Just id),
    onpointerup $ if candrop then dropA dragLens id else setState (dragLens .~ Nothing)  -- stopPropagation
] where
    draggedItem = state ^. dragLens
    candrop = droppable && (draggedItem # maybe false (\x -> canPlay state { from: x, to: id }))
    dragged = draggable && draggedItem == Just id

turnMessage :: ∀pos ext mov. Game pos ext mov => GState pos ext -> String
turnMessage state =
    if isLevelFinished state then
        "Partie finie"
    else if state^._turn == Turn1 then
        "Tour du premier joueur"
    else if state^._mode == DuelMode then
        "Tour du second joueur"
    else 
        "Tour de l'IA"

winTitleFor2Players :: ∀pos ext. GState pos ext -> String
winTitleFor2Players state =
    if state^._mode == DuelMode then
        "Le " <> (if state^._turn == Turn2 then "premier" else "second") <> " joueur gagne"
    else if state^._turn == Turn2 then
        "Tu as gagné"
    else
        "L'IA gagne"
 