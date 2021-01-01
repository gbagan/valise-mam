module Game.Queens.View (view) where

import MyPrelude
import Lib.Util (map2, map3)
import Data.Array.NonEmpty as N
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Keyed as K
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc)
import Game.Core (_position, _nbRows, _nbColumns, _help, _pointer)
import Game.Queens.Model (State, Msg(..), Piece(..),
                           _selectedPiece, _selectedSquare, _allowedPieces, _multiPieces, _customLocalMoves, _customDirections,
                           piecesList, capturableSquares, attackedBySelected)
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

square ∷ ∀a. { piece ∷ Piece, capturable ∷ Boolean, selected ∷ Boolean, nonavailable ∷ Boolean} → Array (H.Prop a) → Html a
square { piece, capturable, selected, nonavailable} props =
    H.div (props <> 
        [   H.class_ "queens-square"
        ,   H.class' "queens-square-capturable" capturable
        ,   H.class' "queens-square-nonavailable" nonavailable
        ,   H.class' "queens-square-selected" selected
        ]) $
        if piece == Empty then [] else [
            H.svg [P.width "100%", P.height "100%", P.viewBox 0 0 100 100, H.class_ "queens-piece"] [
                H.use [P.href $ "#piece-" <> show piece, P.x 10.0, P.y 10.0, P.width "80", P.height "80"]
            ]
        ]

view ∷ State → Html Msg
view state = template {config, board, rules, customDialog, scoreDialog} state where
    position = state ^. _position
    rows = state ^. _nbRows
    columns = state ^. _nbColumns
    allowedPieces = state ^. _allowedPieces
    multiPieces = state ^. _multiPieces
    selectedPiece = state ^. _selectedPiece
    selectedSquare = state ^. _selectedSquare
    help = state ^. _help
    pointer = state ^. _pointer
    customLocalMoves = state ^. _customLocalMoves
    customDirections = state ^. _customDirections

    config =
        card "Les reines"
        [   iconSizesGroup state [4∧4, 5∧5, 7∧7, 8∧8] true
        ,   iconSelectGroupM state "Pièces disponibles" piecesList allowedPieces SelectAllowedPiece \piece →
                _{  icon = IconSymbol $ "#piece-" <> show piece
                ,   tooltip = Just $ tooltip piece
                }
        ,   icongroup "Options" $ 
            [   iconbutton state
                {   icon: IconSymbol "#customize"
                ,   selected: N.head allowedPieces == Custom
                ,   tooltip: Just "Crée ta propre propre pièce"
                }
                [E.onclick Customize]
            ,   iconbutton state
                {   icon: IconSymbol "#piece-mix"
                ,   selected: multiPieces
                ,   tooltip: Just "Mode mixte"
                } [
                    E.onclick ToggleMultiPieces
                ]
            ] <> [ihelp state, ireset state, irules state]
        ,   iconBestScore state
        ]   

    pieceSelector =
        K.div [H.class_ "ui-flex-center gutter2 queens-pieceselector"] $
            N.toArray allowedPieces <#> \piece →
                let name = show piece in
                name /\ iconbutton state
                {   selected: piece == selectedPiece
                ,   icon: IconSymbol $ "#piece-" <> name
                } 
                [   E.onclick $ SelectPiece piece
                ]

    cursor pp =
        H.div ([H.class_ "ui-cursor"] <> cursorStyle pp rows columns 0.8)
        [   H.svg [P.width "100%", P.height "100%"]
            [   H.use [P.href $ "#piece-" <> show selectedPiece]
            ]
        ]

    grid = 
        H.div ([H.class_ "ui-board"] <> gridStyle rows columns 5 <> trackPointer) $ concat
        [   map3 position (attackedBySelected state) (capturableSquares state) \index piece attacked capturable →
                square
                {   piece
                ,   selected: attacked || selectedSquare == Just index
                ,   nonavailable: help && (piece ≠ Empty || capturable)
                ,   capturable
                }
                [   H.style "width" $ pc (1.0 / toNumber columns)
                ,   H.style "height" $ pc (1.0 / toNumber rows)
                ,   E.onclick $ Play index
                ,   E.onpointerenter $ SelectSquare (Just index)
                ,   E.onpointerleave $ SelectSquare Nothing
                ]
        ,   [H.maybe pointer cursor]
        ]

    board = 
        H.div []
        [   pieceSelector
        ,   incDecGrid state [grid]
        ]

    angles = [45, 90, 135, 0, 0, 180, -45, -90, -135]

    customDialog _ = 
        dialog "Personnalise ta pièce"
        [   H.div [H.class_ "flex queens-custompiece"]
            [   K.div [H.class_ "queens-grid queens-custompiece-grid"] (
                    customLocalMoves # mapWithIndex \index selected →
                        show index /\ square
                        {   piece: if index == 12 then Custom else Empty
                        ,   selected: selected
                        ,   capturable: false
                        ,   nonavailable: false
                        } 
                        [   H.style "width" "20%"
                        ,   H.style "height" "20%"
                        ,   E.onclick' if index ≠ 12 then Just (FlipLocalMove index) else Nothing
                        ]
                )
            ,   H.div [H.class_ "flex  queens-custompiece-directions"] (
                    map2 customDirections angles \i selected angle →
                        iconbutton state 
                        {   selected: selected
                        ,       icon: if i == 4 then IconNone else IconSymbol "#arrow"
                        ,   style: ["transform" ∧ ("rotate(" <> show angle <> "deg)")]
                        }
                        [   E.onclick' $ if i ≠ 4 then Just (FlipDirection i) else Nothing
                        ]
            )
        ]
    ]    
        
    scoreDialog _ = bestScoreDialog state \pos → [
        H.div [H.class_ "ui-flex-center queens-bestscore-container"] [
            H.div (gridStyle rows columns 5 <> [H.class_ "ui-board queens-grid"]) (
                pos <#> \piece →
                    square { piece, capturable: false, selected: false, nonavailable: false}
                    [   H.style "width" $ pc (1.0 / toNumber columns)
                    ,   H.style "height" $ pc (1.0 / toNumber rows)
                    ]
            )
        ]
    ]

    rules = 
        [   H.text "Place le plus de pièces possible sur ta grille sans qu\'aucune ne soit menacée par une autre pièce."
        ,   H.br
        ,   H.text "Tu peux choisir de jouer avec différentes pièces comme celles du jeu d\'échecs."
        ,   H.br
        ,   H.text "Le mode mixte permet de jouer avec plusieurs pièces différentes."
        ,   H.br
        ,   H.text "Tu peux jouer avec une pièce personnalisée si tu le souhaites."
        ]
