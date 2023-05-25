module Game.Jetons.View (view) where

import MamPrelude
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Util (pc, rgbColor)
import Game.Core (_position, _nbColumns, _nbRows, _pointer, scoreFn)
import Game.Jetons.Model (Model, Msg, _dragged)
import Lib.Helpers (coords)
import UI.Template (template, card, bestScoreDialog, incDecGrid, gridStyle, dndBoardProps, dndItemProps, cursorStyle)
import UI.Icons (icongroup, iconBestScore, iconSizesGroup, iundo, iredo, ireset, irules)

view ∷ Model → Html Msg
view model = template { config, board, rules, winTitle, scoreDialog } model
  where
  position = model ^. _position
  columns = model ^. _nbColumns
  rows = model ^. _nbRows
  dragged = model ^. _dragged
  pointer = model ^. _pointer

  config = card "Jeu d'acquisition"
    [ iconSizesGroup model [ 2 ∧ 2, 4 ∧ 4, 5 ∧ 5, 6 ∧ 6 ] true
    , icongroup "Options" $ [ iundo, iredo, ireset, irules ] <#> (_ $ model)
    , iconBestScore model
    ]

  cursor pp _ = H.div ([ H.class_ "ui-cursor jetons-cursor" ] <> cursorStyle pp rows columns 0.6) []

  piece i val props =
    let
      { row, col } = coords columns i
    in
      H.div
        ( [ H.class_ "jetons-peg"
          , H.class' "small" $ columns >= 8
          , H.style "background-color" $ rgbColor 255 (floor $ 255.0 * (1.0 - sqrt (toNumber val / toNumber (rows * columns)))) 0
          , H.style "left" $ pc $ (0.15 + toNumber col) / toNumber columns
          , H.style "top" $ pc $ (0.15 + toNumber row) / toNumber rows
          , H.style "width" $ pc $ 0.7 / toNumber columns
          , H.style "height" $ pc $ 0.7 / toNumber rows
          , H.style "box-shadow" $ show (val * 2) <> "px " <> show (val * 2) <> "px 5px 0px #656565"
          ] <> props
        )
        [ H.span [] [ H.text $ show val ] ]

  board = incDecGrid model
    [ H.div ([ H.class_ "ui-board" ] <> dndBoardProps <> gridStyle rows columns 3) $ concat
        [ position # mapWithIndex \i val →
            H.when (val ≠ 0) \_ →
              piece i val
                ( [] <>
                    dndItemProps model
                      { currentDragged: dragged
                      , draggable: true
                      , droppable: true
                      , id: i
                      }
                )
        , [ H.fromMaybe $ cursor <$> pointer <*> dragged ]
        ]
    ]

  scoreDialog _ = bestScoreDialog model \pos →
    [ H.div [ H.class_ "ui-flex-center jetons-bestscore-grid-container" ]
        [ H.div (gridStyle rows columns 3 <> [ H.class_ "ui-board" ])
            ( pos # mapWithIndex \i val →
                H.when (val ≠ 0) \_ →
                  piece i val []
            )
        ]
    ]

  rules =
    [ H.text "À chaque tour de ce jeu, tu peux déplacer une pile de jetons vers une case adjacente qui contient au moins autant de jetons."
    , H.br
    , H.text "Le but est de finir la partie avec le moins de cases contenant des piles de jetons."
    ]

  score = scoreFn model
  s = if score > 1 then "s" else ""
  winTitle = show score <> " case" <> s <> " restante" <> s