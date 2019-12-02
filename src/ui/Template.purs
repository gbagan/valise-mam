module UI.Template where
import MyPrelude
import Pha (VDom, Prop, text, emptyNode, (<??>), class_, class', style)
import Pha.Elements (div)
import Pha.Events (on, onpointerup, onpointerup', onpointerleave, releasePointerCaptureOn)
import Pha.Events.Decoder (always)
import Pha.Util (pc, translate)
import Game.Core (class Game, class ScoreGame, GState, Mode(..), Turn(..), SizeLimit(..), Dialog(..),
         _dialog, _nbColumns, _nbRows, _customSize, _mode, _turn, _showWin, _locked, 
        isLevelFinished, sizeLimit, bestScore, canPlay,
        class MsgWithCore, core, CoreMsg(..), class MsgWithDnd, dndmsg, DndMsg(..)
        )
import Game.Common (pointerDecoder)
import Lib.Util (partialUpdate, class PartialRecord)
import UI.Dialog (dialog) as D
import UI.IncDecGrid (incDecGrid) as U
type Position = { x :: Number, y :: Number }
-- import Game.Effs (Position)

winPanel :: ∀a. String -> Boolean -> VDom a
winPanel title visible =
    div [class_ "ui-flex-center ui-absolute component-win-container"] [
        div [class_ "component-win", class' "visible" visible] [
            text title
        ]
    ]

card :: ∀a. String -> Array (VDom a) -> VDom a
card title children =
    div [class_ "ui-card"] [
        div [class_ "ui-card-head ui-flex-center"] [
            div [class_ "ui-card-title"] [text title]
        ],
        div [class_ "ui-card-body"] children
    ]

incDecGrid :: ∀msg pos ext mov. MsgWithCore msg => Game pos ext mov =>
    GState pos ext -> Array (VDom msg) -> VDom msg
incDecGrid state = U.incDecGrid {
    locked: state^._locked,
    nbRows: state^._nbRows,
    nbColumns: state^._nbColumns,
    showRowButtons: minRows < maxRows,
    showColButtons: minCols < maxCols,
    customSize: state^._customSize,
    resize: \x y -> core (SetGridSize x y true)
} where
    SizeLimit minRows minCols maxRows maxCols = sizeLimit state 

type ElementsRow a = (
    board :: VDom a,
    config :: VDom a,
    rules :: Array (VDom a),
    winTitle :: String,
    customDialog :: Unit -> VDom a,
    scoreDialog :: Unit -> VDom a
)

type Elements a = { | ElementsRow a }

defaultElements :: ∀a. Elements a
defaultElements = {
    board: emptyNode,
    config: emptyNode,
    rules: [text "blah blah"],
    winTitle: "GAGNÉ",
    customDialog: \_ -> emptyNode,
    scoreDialog: \_ -> emptyNode
}

template :: ∀elems pos ext mov msg.
            PartialRecord elems (ElementsRow msg) =>
            MsgWithCore msg => Game pos ext mov =>
            Record (elems) -> GState pos ext -> VDom msg
template elems state =
    div [] [
        div [class_ "main-container"] [
            div [] [board, winPanel winTitle (state^._showWin)],
            config
        ],
        dialog' (state^._dialog)
    ]
    where
        {config, board, winTitle, rules, customDialog, scoreDialog} = partialUpdate elems defaultElements
        dialog' Rules = dialog "Règles du jeu" rules
        dialog' (ConfirmNewGameDialog _) =
            D.dialog {
                title: "Nouvelle partie", 
                onCancel: Just $ core SetNoDialog, 
                onOk: Just $ core ConfirmNewGame
            } [
                text "Tu es sur le point de créer une nouvelle partie. Ta partie en cours sera perdue. Es-tu sûr(e)?"
            ]
        dialog' CustomDialog = customDialog unit
        dialog' ScoreDialog = scoreDialog unit
        dialog' _ = emptyNode

dialog :: ∀msg. MsgWithCore msg => String -> Array (VDom msg) -> VDom msg
dialog title = D.dialog {title, onCancel: Nothing, onOk: Just $ core SetNoDialog}

bestScoreDialog :: ∀msg pos ext mov. MsgWithCore msg => ScoreGame pos ext mov =>  
                    GState pos ext -> (pos -> Array (VDom msg)) -> VDom msg
bestScoreDialog state children = snd <$> bestScore state <??> \pos ->
    dialog "Meilleur score" (children pos)
        

gridStyle :: ∀a. Int -> Int -> Int -> Array (Prop a)
gridStyle rows columns limit = [style "height" $ pc (toNumber rows / m),
                                style "width" $ pc (toNumber columns / m)]
    where m = toNumber $ max limit $ max rows columns        

cursorStyle :: ∀a. Position -> Int -> Int -> Number -> Array (Prop a)
cursorStyle {x, y} rows columns size = [
    style "left" $ pc x,
    style "top" $ pc y,
    style "width" $ pc (size / toNumber columns),
    style "height" $ pc (size / toNumber rows)
]

svgCursorStyle :: ∀a. Position -> Array (Prop a)
svgCursorStyle {x, y} = [
    style "transform" $ translate (pc x) (pc y)
]

-- style à appliquer sur l'élément DOM représentant le plateau
-- permet de mémoriser la position du pointeur
trackPointer :: ∀msg. MsgWithCore msg => Array (Prop msg)
trackPointer = [
    style "touch-action" "none",
    on "pointermove" move,
    onpointerleave $ core (SetPointer Nothing),
    on "pointerdown" move
] where
    move e = core <$> (SetPointer <$> Just <$> pointerDecoder e)

-- même chose que trackPointer mais gère le drag and drop par l'intermédiaire d'un lens
dndBoardProps :: ∀msg id. MsgWithCore msg => MsgWithDnd msg id => Array (Prop msg)
dndBoardProps = [
    style "touch-action" "none", 
    on "pointermove" move,
    onpointerup $ dndmsg DropOnBoard,
    onpointerleave $ dndmsg Leave
] where
    move e = core <$> (SetPointer <$> Just <$> pointerDecoder e)

dndItemProps :: ∀pos ext msg id. Eq id => MsgWithDnd msg id => Game pos ext {from :: id, to :: id} =>
    {
        draggable :: Boolean,
        droppable :: Boolean,
        id :: id,
        currentDragged :: Maybe id
    } -> (GState pos ext) -> Array (Prop msg)
dndItemProps {draggable, droppable, id, currentDragged} state = [
    class' "dragged" dragged,
    class' "candrop" candrop,
    releasePointerCaptureOn "pointerdown" $ always (if draggable then Just $ dndmsg (Drag id) else Nothing),
    onpointerup' $ if candrop then Just $ dndmsg (Drop id) else Nothing -- todo ne rien faire  -- stopPropagation
] where
    candrop = droppable && (currentDragged # maybe false \d -> canPlay state {from: d, to: id})
    dragged = draggable && Just id == currentDragged

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
 