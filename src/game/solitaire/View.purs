module Game.Solitaire.View where

import MamPrelude
import Lib.Helpers (coords)
import Data.FoldableWithIndex (foldMapWithIndex)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Util (translate)
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import Game.Core (PointerPosition, _position, _nbColumns, _nbRows, _pointer, scoreFn)
import Game.Solitaire.Model (Model, Msg(..), Board(..), _board, _holes, _dragged, _help)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup', iconBestScore, ihelp, iundo, iredo, ireset, irules)
import UI.Template (template, card, bestScoreDialog, gridStyle, incDecGrid, svgCursorStyle, dndBoardProps, dndItemProps)

tricolor ∷ Int → Int → Int → String
tricolor i columns help =
  case (i `mod` columns + help * (i / columns)) `mod` 3 of
    0 → "red"
    1 → "blue"
    _ → "green"

cursor ∷ ∀ a b. PointerPosition → b → Html a
cursor pp _ = S.circle ([ SA.r 20.0, H.class_ "solitaire-cursor" ] <> svgCursorStyle pp)

view ∷ Model → Html Msg
view model = template { config, board, rules, winTitle, scoreDialog } model
  where
  position = model ^. _position
  columns = model ^. _nbColumns
  rows = model ^. _nbRows
  isCircleBoard = model ^. _board == CircleBoard
  board_ = model ^. _board
  help = model ^. _help
  pointer = model ^. _pointer
  dragged = model ^. _dragged
  holes = model ^. _holes

  itemStyle i =
    let
      { row, col } = coords columns i
    in
      if isCircleBoard then
        translate
          (show $ 125.0 + sin (2.0 * pi * toNumber i / toNumber rows) * 90.0)
          (show $ 125.0 + cos (2.0 * pi * toNumber i / toNumber rows) * 90.0)
      else
        translate
          (show $ 50 * col + 25)
          (show $ 50 * row + 25)

  config =
    card "Jeu du solitaire"
      [ iconSelectGroup' model "Plateau" board_ SetBoard
          [ CircleBoard ∧ _ { icon = IconSymbol "#circle", tooltip = Just "Cercle" }
          , Grid3Board ∧ _ { icon = IconText "3xN", tooltip = Just "3xN" }
          , RandomBoard ∧ _ { icon = IconSymbol "#shuffle", tooltip = Just "Aléatoire" }
          , EnglishBoard ∧ _ { icon = IconSymbol "#tea", tooltip = Just "Anglais" }
          , FrenchBoard ∧ _ { icon = IconSymbol "#bread", tooltip = Just "Français" }
          ]
      , icongroup "Options" $ [ ihelp, iundo, iredo, ireset, irules ] # map (_ $ model)
      , iconBestScore model
      ]

  drawHole i =
    [ H.when (help > 0 && not isCircleBoard) \_ →
        S.rect
          [ SA.x (-25.0)
          , SA.y (-25.0)
          , SA.width 50
          , SA.height 50
          , SA.fill $ tricolor i columns help
          , SA.transform $ itemStyle i
          ]
    , S.circle
        ( [ SA.r 17.0
          , H.class_ "solitaire-hole"
          , SA.transform $ itemStyle i
          ] <> dndItemProps model
            { currentDragged: dragged
            , draggable: false
            , droppable: true
            , id: i
            }
        )
    ]

  drawPeg i =
    S.circle
      ( [ SA.r 20.0
        , H.class_ "solitaire-peg"
        , SA.transform $ itemStyle i
        ] <> dndItemProps model
          { draggable: true
          , droppable: false
          , currentDragged: dragged
          , id: i
          }
      )

  grid =
    H.div
      ( [ H.class_ "ui-board" ]
          <> dndBoardProps
          <>
            ( if isCircleBoard then
                [ H.style "width" "100%", H.style "height" "100%" ]
              else
                gridStyle rows columns 5
            )
      )
      [ S.svg 
          [ if isCircleBoard then
              SA.viewBox 0.0 0.0 250.0 250.0
            else SA.viewBox 0.0 0.0 (50.0 * toNumber columns) (50.0 * toNumber rows) 
          ] $ concat
          [ [ H.when isCircleBoard \_ →
                S.circle [ SA.cx 125.0, SA.cy 125.0, SA.r 90.0, H.class_ "solitaire-circle" ]
            ]
          , holes # foldMapWithIndex \i hasHole →
              if hasHole then drawHole i else []
          , position # mapWithIndex \i hasPeg →
              H.when hasPeg \_ → drawPeg i
          , [ H.fromMaybe (cursor <$> pointer <*> dragged) ]
          ]
      ]

  board = incDecGrid model [ grid ]

  scoreDialog _ = bestScoreDialog model \bestPosition →
    [ H.div [ H.class_ "ui-flex-center solitaire-scoredialog" ]
        [ H.div
            ( [ H.class_ "ui-board" ] <>
                ( if isCircleBoard then
                    [ H.style "width" "100%", H.style "height" "100%" ]
                  else
                    gridStyle rows columns 5
                )
            )
            [ S.svg 
                [ if isCircleBoard then
                    SA.viewBox 0.0 0.0 250.0 250.0
                  else
                    SA.viewBox 0.0 0.0 (50.0 * toNumber columns) (50.0 * toNumber rows)
                ]
                [ H.when isCircleBoard \_ →
                    S.circle
                      [ SA.cx 125.0
                      , SA.cy 125.0
                      , SA.r 90.0
                      , H.class_ "solitaire-circle"
                      ]
                , S.g []
                    $ holes
                    # mapWithIndex \i b → H.when b \_ →
                        S.circle
                          [ SA.r 17.0
                          , H.class_ "solitaire-hole"
                          , SA.transform $ itemStyle i
                          ]
                , S.g []
                    $ bestPosition
                    # mapWithIndex \i b → H.when b \_ →
                        S.circle
                          [ SA.r 20.0
                          , H.class_ "solitaire-peg"
                          , SA.transform $ itemStyle i
                          ]
                ]
            ]
        ]
    ]

  rules = [ H.text "Jeu du solitaire", H.br, H.text "blah blah" ]

  nbPegs = scoreFn model
  s = if nbPegs > 1 then "s" else ""
  winTitle = show nbPegs <> " jeton" <> s <> " restant" <> s