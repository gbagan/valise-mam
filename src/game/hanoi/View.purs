module Game.Hanoi.View (view) where

import MamPrelude
import Game.Core (_position, _pointer, _history)
import Game.Hanoi.Model (Model, Msg(..), _dragged, _nbDisks)
import Data.List as List
import Pha.Html (Html, Prop)
import Pha.Html as H
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)
import UI.Template (template, card, dndBoardProps, dndItemProps)

colors ∷ Array String
colors = [ "blue", "red", "green", "magenta", "orange", "gray", "cyan" ]

drawTower ∷ ∀ a. Int → Html a
drawTower i = S.path
  [ SA.d $ "M" <> show (i * 60 + 14) <> " 99a3 3 0 0 1 0 -6h20a3 3 0 0 0 3 -3v-80a3 3 0 0 1 6 0v80"
      <> "a3 3 0 0 0 3 3h20a3 3 0 0 1 0 6z"
  , SA.stroke "blue"
  , SA.strokeWidth 0.5
  , SA.fill "#d43"
  ]

view ∷ Model → Html Msg
view model = template { config, board, rules, winTitle } model
  where
  position = model ^. _position
  nbDisks = model ^. _nbDisks
  dragged = model ^. _dragged
  pointer = model ^. _pointer
  nbMoves = List.length $ model ^. _history

  config =
    card "Tours de Hanoi"
      [ iconSelectGroup model "Nombres de disques" [ 3, 4, 5, 6, 7 ] nbDisks SetNbDisks (const identity)
      , icongroup "Options" $ [ iundo, iredo, ireset, irules ] # map (_ $ model)
      ]

  drawDisk ∷ { x ∷ Number, y ∷ Number, disk ∷ Int, column ∷ Int, draggable ∷ Boolean } → Array (Prop Msg) → Html Msg
  drawDisk { x, y, disk, column, draggable } props =
    let
      color = colors !! disk ?: "black"
    in
      S.rect
        $
          [ SA.x $ x - 25.0 + 2.5 * toNumber disk
          , SA.y $ y - 7.0
          , SA.width $ 50 - 5 * disk
          , SA.height 10
          , SA.rx 5
          , SA.ry 5
          , H.class_ "hanoi-disk"
          , SA.fill color
          ]
        <> props
        <> dndItemProps model
          { currentDragged: dragged
          , draggable
          , droppable: false
          , id: column
          }

  cursor ∷ { x ∷ Number, y ∷ Number } → Int → Html Msg
  cursor { x, y } disk =
    drawDisk
      { x: x * 200.0
      , y: y * 100.0
      , disk
      , column: -1
      , draggable: false
      }
      [ H.style "pointer-events" "none" ]

  board ∷ Html Msg
  board =
    H.div (dndBoardProps <> [ H.class_ "ui-board hanoi-board" ])
      [ S.svg [ H.class_ "hanoi-svg", SA.viewBox 0.0 0.0 200.0 100.0 ] $
          [ S.g [] $ [ 0, 1, 2 ] # map drawTower
          , S.g [] $ [ 0, 1, 2 ] # map \i →
              S.rect
                ( [ SA.x $ 13 + 60 * i, SA.y 10.0, SA.width 54, SA.height 90, SA.fill "transparent" ]
                    <> dndItemProps model
                      { currentDragged: dragged
                      , draggable: false
                      , droppable: true
                      , id: i
                      }
                )
          , S.g [] $ concat
              ( position # mapWithIndex \i stack →
                  stack # mapWithIndex \j k →
                    drawDisk
                      { x: toNumber $ 40 + 60 * i
                      , y: toNumber $ 90 - 10 * j
                      , disk: k
                      , column: i
                      , draggable: j == length stack - 1
                      }
                      []
              )
          , H.fromMaybe $ cursor <$> pointer <*> (dragged >>= (position !! _) >>= last)
          ]
      , H.div [ H.class_ "hanoi-nbmoves" ] [ H.text $ show nbMoves <> " coup" <> (if nbMoves > 1 then "s" else "") ]
      ]

  rules =
    [ H.text "Le but du jeu est de déplacer tous les disques sur la tour de droite avec les contraintes suivantes:"
    , H.br
    , H.text "- tu peux déplacer seulement un disque à la fois;"
    , H.br
    , H.text "- tu ne peux pas déplacer un disque sur un disque plus petit que lui."
    ]

  winTitle = "Tu as gagné en " <> show nbMoves <> " coups"