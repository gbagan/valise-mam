module UI.Template where

import MamPrelude

import Game.Core (class Game, class ScoreGame, GModel, Mode(..), Turn(..), SizeLimit(..), Dialog(..), _dialog, _nbColumns, _nbRows, _customSize, _mode, _turn, _showWin, _locked, isLevelFinished, sizeLimit, bestScore, canPlay, class MsgWithCore, core, CoreMsg(..), class MsgWithDnd, dndmsg, DndMsg(..))
import Game.Helpers (pointerDecoder, releasePointerCapture)
import Lib.Helpers (partialUpdate, class PartialRecord)
import Pha.Html (Html, Prop)
import Pha.Html as H
import Pha.Html.Events as E
import Pha.Html.Util (pc, translate)
import UI.Dialog (dialog) as D
import UI.IncDecGrid (incDecGrid) as U
import Web.Event.Event (stopPropagation)
import Web.PointerEvent.PointerEvent as PE

type Position = { x ∷ Number, y ∷ Number }

-- | widget pour l'affichage du panneau de victoire
winPanel ∷ ∀ a. String → Boolean → Html a
winPanel title visible =
  H.div [ H.class_ "ui-flex-center ui-absolute ui-win-container" ]
    [ H.div [ H.class_ "ui-win", H.class' "visible" visible ]
        [ H.text title
        ]
    ]

-- | widget principal pour l'affichage du panneau des options
card ∷ ∀ a. String → Array (Html a) → Html a
card title children =
  H.div [ H.class_ "ui-card" ]
    [ H.div [ H.class_ "ui-card-head ui-flex-center" ]
        [ H.div [ H.class_ "ui-card-title" ] [ H.text title ]
        ]
    , H.div [ H.class_ "ui-card-body" ] children
    ]

-- | widget permettant de changer les dimensions d'un plateau 2D
incDecGrid
  ∷ ∀ msg pos ext mov
  . MsgWithCore msg
  ⇒ Game pos ext mov
  ⇒ GModel pos ext
  → Array (Html msg)
  → Html msg
incDecGrid model = U.incDecGrid
  { locked: model ^. _locked
  , nbRows: model ^. _nbRows
  , nbColumns: model ^. _nbColumns
  , showRowButtons: minRows < maxRows
  , showColButtons: minCols < maxCols
  , customSize: model ^. _customSize
  , resize: \x y → core (SetGridSize x y true)
  }
  where
  SizeLimit minRows minCols maxRows maxCols = sizeLimit model

type ElementsRow a =
  ( board ∷ Html a
  , config ∷ Html a
  , rules ∷ Array (Html a)
  , winTitle ∷ String
  , customDialog ∷ Unit → Html a
  , scoreDialog ∷ Unit → Html a
  )

type Elements a = Record (ElementsRow a)

defaultElements ∷ ∀ a. Elements a
defaultElements =
  { board: H.empty
  , config: H.empty
  , rules: [ H.text "blah blah" ]
  , winTitle: "GAGNÉ"
  , customDialog: const H.empty
  , scoreDialog: const H.empty
  }

template
  ∷ ∀ elems pos ext mov msg
  . PartialRecord elems (ElementsRow msg)
  ⇒ MsgWithCore msg
  ⇒ Game pos ext mov
  ⇒ Record (elems)
  → GModel pos ext
  → Html msg
template elems model =
  H.div []
    [ H.div [ H.class_ "main-container" ]
        [ H.div [] [ board, winPanel winTitle (model ^. _showWin) ]
        , config
        ]
    , dialog' (model ^. _dialog)
    ]
  where
  { config, board, winTitle, rules, customDialog, scoreDialog } = partialUpdate elems defaultElements
  dialog' Rules = dialog "Règles du jeu" [ H.div [ H.class_ "ui-rules" ] rules ]
  dialog' (ConfirmNewGameDialog _) =
    D.dialog
      { title: "Nouvelle partie"
      , onCancel: Just $ core SetNoDialog
      , onOk: Just $ core ConfirmNewGame
      }
      [ H.text "Tu es sur le point de créer une nouvelle partie. Ta partie en cours sera perdue. Es-tu sûr(e)?"
      ]
  dialog' CustomDialog = customDialog unit
  dialog' ScoreDialog = scoreDialog unit
  dialog' _ = H.empty

dialog ∷ ∀ msg. MsgWithCore msg ⇒ String → Array (Html msg) → Html msg
dialog title = D.dialog { title, onCancel: Nothing, onOk: Just $ core SetNoDialog }

bestScoreDialog
  ∷ ∀ msg pos ext mov
  . MsgWithCore msg
  ⇒ ScoreGame pos ext mov
  ⇒ GModel pos ext
  → (pos → Array (Html msg))
  → Html msg
bestScoreDialog model children = H.maybe (snd <$> bestScore model) \pos →
  dialog "Meilleur score" (children pos)

-- | Fonction utilaire pour définir le style d'un plateau 2D par rapport à ses dimensions et une limite.
-- | Le plateau essaie de prendre toute la place à sa disposition sauf si ses deux dimensions sont inférieure à la limite
gridStyle ∷ ∀ a. Int → Int → Int → Array (Prop a)
gridStyle rows columns limit =
  [ H.style "height" $ pc (toNumber rows / m)
  , H.style "width" $ pc (toNumber columns / m)
  ]
  where
  m = toNumber $ max limit $ max rows columns

cursorStyle ∷ ∀ a. Position → Int → Int → Number → Array (Prop a)
cursorStyle { x, y } rows columns size =
  [ H.style "left" $ pc x
  , H.style "top" $ pc y
  , H.style "width" $ pc (size / toNumber columns)
  , H.style "height" $ pc (size / toNumber rows)
  ]

svgCursorStyle ∷ ∀ a. Position → Array (Prop a)
svgCursorStyle { x, y } =
  [ H.style "transform" $ translate (pc x) (pc y)
  ]

-- | tableau d'atrributs à appliquer sur l'élément DOM représentant le plateau
-- | permet de mémoriser la position du pointeur
trackPointer ∷ ∀ msg. MsgWithCore msg ⇒ Array (Prop msg)
trackPointer =
  [ E.onPointerMove' $ pointerDecoder (core <<< SetPointer) <<< PE.toMouseEvent
  , E.onPointerLeave \_ → core SetPointerToNothing
  , E.onPointerDown' $ pointerDecoder (core <<< SetPointer) <<< PE.toMouseEvent
  ]

-- | même chose que trackPointer mais gère le drag and drop par l'intermédiaire d'un lens
dndBoardProps ∷ ∀ msg id. MsgWithCore msg ⇒ MsgWithDnd msg id ⇒ Array (Prop msg)
dndBoardProps =
  [ E.onPointerDown' $ pointerDecoder (core <<< SetPointer) <<< PE.toMouseEvent
  , E.onPointerMove' $ pointerDecoder (core <<< SetPointer) <<< PE.toMouseEvent
  , E.onPointerUp \_ → dndmsg DropOnBoard
  , E.onPointerLeave \_ → dndmsg Leave
  ]

-- | gère le drag and drop pour une item en renvoyant une liste d'attributs
dndItemProps
  ∷ ∀ pos ext msg id
  . Eq id
  ⇒ MsgWithDnd msg id
  ⇒ Game pos ext { from ∷ id, to ∷ id }
  ⇒ (GModel pos ext)
  → { draggable ∷ Boolean
    , droppable ∷ Boolean
    , id ∷ id
    , currentDragged ∷ Maybe id
    }
  → Array (Prop msg)
dndItemProps model { draggable, droppable, id, currentDragged } =
  [ H.class' "dragged" dragged
  , H.class' "candrop" candrop
  , E.onPointerDown' \ev → do
      releasePointerCapture ev
      pure $ Just $ dndmsg (Drag draggable id)
  , E.onPointerUp' \ev → do
      if candrop then do
        stopPropagation $ PE.toEvent ev
        pure $ Just $ dndmsg (Drop id)
      else
        pure Nothing
  ]
  where
  candrop = droppable && (currentDragged # maybe false \d → canPlay model { from: d, to: id })
  dragged = draggable && Just id == currentDragged

-- | un message qui indique à qui est le tour ou si la partie est finie
turnMessage ∷ ∀ pos ext mov. Game pos ext mov ⇒ GModel pos ext → String
turnMessage model =
  if isLevelFinished model then
    "Partie finie"
  else if model ^. _turn == Turn1 then
    "Tour du premier joueur"
  else if model ^. _mode == DuelMode then
    "Tour du second joueur"
  else
    "Tour de l'IA"

-- | un message de fin de partie pour les jeux à deux joueurs
winTitleFor2Players ∷ ∀ pos ext. GModel pos ext → String
winTitleFor2Players model =
  if model ^. _mode == DuelMode then
    "Le " <> (if model ^. _turn == Turn2 then "premier" else "second") <> " joueur gagne"
  else if model ^. _turn == Turn2 then
    "Tu as gagné"
  else
    "L'IA gagne"
