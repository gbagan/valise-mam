module Game.Queens.View (view) where

import MamPrelude
import Lib.Helpers (map2, map3)
import Data.Array.NonEmpty as N
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc)
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import Game.Core (_position, _nbRows, _nbColumns, _help, _pointer)
import Game.Queens.Model
  ( Model
  , Msg(..)
  , Piece(..)
  , _selectedPiece
  , _selectedSquare
  , _allowedPieces
  , _multiPieces
  , _customLocalMoves
  , _customDirections
  , piecesList
  , capturableSquares
  , attackedBySelected
  )
import UI.Template (template, card, dialog, bestScoreDialog, incDecGrid, gridStyle, trackPointer, cursorStyle)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSizesGroup, iconSelectGroupM, iconBestScore, ihelp, irules, ireset)

tooltip ∷ Piece → String
tooltip Queen = "Reine"
tooltip King = "Roi"
tooltip Rook = "Tour"
tooltip Bishop = "Fou"
tooltip Knight = "Cavalier"
tooltip _ = "Pièce personnalisée"

square ∷ ∀ a. { piece ∷ Piece, capturable ∷ Boolean, selected ∷ Boolean, nonavailable ∷ Boolean } → Array (H.Prop a) → Html a
square { piece, capturable, selected, nonavailable } props =
  H.div
    ( props <>
        [ H.class_ "queens-square"
        , H.class' "queens-square-capturable" capturable
        , H.class' "queens-square-nonavailable" nonavailable
        , H.class' "queens-square-selected" selected
        ]
    ) $
    if piece == Empty then []
    else
      [ S.svg [ H.style "width" "100%", H.style "height" "100%", SA.viewBox 0.0 0.0 100.0 100.0, H.class_ "queens-piece" ]
          [ S.use [ P.href $ "#piece-" <> show piece, SA.x 10, SA.y 10, SA.width 80, SA.height 80 ]
          ]
      ]

view ∷ Model → Html Msg
view model = template { config, board, rules, customDialog, scoreDialog } model
  where
  position = model ^. _position
  rows = model ^. _nbRows
  columns = model ^. _nbColumns
  allowedPieces = model ^. _allowedPieces
  multiPieces = model ^. _multiPieces
  selectedPiece = model ^. _selectedPiece
  selectedSquare = model ^. _selectedSquare
  help = model ^. _help
  pointer = model ^. _pointer
  customLocalMoves = model ^. _customLocalMoves
  customDirections = model ^. _customDirections

  config =
    card "Les reines"
      [ iconSizesGroup model [ 4 ∧ 4, 5 ∧ 5, 7 ∧ 7, 8 ∧ 8 ] true
      , iconSelectGroupM model "Pièces disponibles" piecesList allowedPieces SelectAllowedPiece \piece →
          _
            { icon = IconSymbol $ "#piece-" <> show piece
            , tooltip = Just $ tooltip piece
            }
      , icongroup "Options" $
          [ iconbutton model
              { icon: IconSymbol "#customize"
              , selected: N.head allowedPieces == Custom
              , tooltip: Just "Crée ta propre pièce"
              }
              [ E.onClick \_ → Customize ]
          , iconbutton model
              { icon: IconSymbol "#piece-mix"
              , selected: multiPieces
              , tooltip: Just "Mode mixte"
              }
              [ E.onClick \_ → ToggleMultiPieces
              ]
          , ihelp model
          , ireset model
          , irules model
          ]
      , iconBestScore model
      ]

  pieceSelector =
    H.div [ H.class_ "ui-flex-center gutter2 queens-pieceselector" ]
      $ N.toArray allowedPieces
      # map \piece →
        let
          name = show piece
        in
          iconbutton model
            { selected: piece == selectedPiece
            , icon: IconSymbol $ "#piece-" <> name
            }
            [ E.onClick \_ → SelectPiece piece
            ]

  cursor pp =
    H.div ([ H.class_ "ui-cursor" ] <> cursorStyle pp rows columns 0.8)
      [ S.svg [ H.style "width" "100%", H.style "height" "100%" ]
          [ S.use [ P.href $ "#piece-" <> show selectedPiece ]
          ]
      ]

  grid =
    H.div ([ H.class_ "ui-board" ] <> gridStyle rows columns 5 <> trackPointer) $ concat
      [ map3 position (attackedBySelected model) (capturableSquares model) \index piece attacked capturable →
          square
            { piece
            , selected: attacked || selectedSquare == Just index
            , nonavailable: help && (piece ≠ Empty || capturable)
            , capturable
            }
            [ H.style "width" $ pc (1.0 / toNumber columns)
            , H.style "height" $ pc (1.0 / toNumber rows)
            , E.onClick \_ → Play index
            , E.onPointerEnter \_ → SelectSquare (Just index)
            , E.onPointerLeave \_ → SelectSquare Nothing
            ]
      , [ H.maybe pointer cursor ]
      ]

  board =
    H.div []
      [ pieceSelector
      , incDecGrid model [ grid ]
      ]

  angles = [ 45, 90, 135, 0, 0, 180, -45, -90, -135 ]

  customDialog _ =
    dialog "Personnalise ta pièce"
      [ H.div [ H.class_ "flex queens-custompiece" ]
          [ H.div [ H.class_ "queens-grid queens-custompiece-grid" ]
              ( customLocalMoves # mapWithIndex \index selected →
                  square
                    { piece: if index == 12 then Custom else Empty
                    , selected: selected
                    , capturable: false
                    , nonavailable: false
                    }
                    [ H.style "width" "20%"
                    , H.style "height" "20%"
                    , E.onClick \_ → FlipLocalMove index
                    ]
              )
          , H.div [ H.class_ "flex  queens-custompiece-directions" ]
              ( map2 customDirections angles \i selected angle →
                  iconbutton model
                    { selected: selected
                    , icon: if i == 4 then IconNone else IconSymbol "#arrow"
                    , style: [ "transform" ∧ ("rotate(" <> show angle <> "deg)") ]
                    }
                    [ E.onClick \_ → FlipDirection i
                    ]
              )
          ]
      ]

  scoreDialog _ = bestScoreDialog model \pos →
    [ H.div [ H.class_ "ui-flex-center queens-bestscore-container" ]
        [ H.div (gridStyle rows columns 5 <> [ H.class_ "ui-board queens-grid" ])
            ( pos # map \piece →
                square { piece, capturable: false, selected: false, nonavailable: false }
                  [ H.style "width" $ pc (1.0 / toNumber columns)
                  , H.style "height" $ pc (1.0 / toNumber rows)
                  ]
            )
        ]
    ]

  rules =
    [ H.text "Place le plus de pièces possible sur ta grille sans qu\'aucune ne soit menacée par une autre pièce."
    , H.br
    , H.text "Tu peux choisir de jouer avec différentes pièces comme celles du jeu d\'échecs."
    , H.br
    , H.text "Le mode mixte permet de jouer avec plusieurs pièces différentes."
    , H.br
    , H.text "Tu peux jouer avec une pièce personnalisée si tu le souhaites."
    ]
