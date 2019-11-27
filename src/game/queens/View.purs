module Game.Queens.View (view) where

import MyPrelude
import Lib.Util (map2, map3)
import Data.Array.NonEmpty (toArray, head) as N
import Pha (VDom, Prop, text, (<??>), class_, class', key, style)
import Pha.Elements (div, br)
import Pha.Events (onclick, onclick', onpointerenter, onpointerleave)
import Pha.Svg (svg, use, x_, y_, width, height)
import Pha.Util (pc)
import Game.Core (_position, _nbRows, _nbColumns, _help, _pointer)
import Game.Queens.Model (State, Msg(..), Piece(..),
                           _selectedPiece, _selectedSquare, _allowedPieces, _multiPieces, _customLocalMoves, _customDirections,
                           piecesList, capturableSquares, attackedBySelected)
import UI.Template (template, card, dialog, bestScoreDialog, incDecGrid, gridStyle, trackPointer, cursorStyle)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSizesGroup, iconSelectGroupM, iconBestScore, ihelp, irules, ireset)

tooltip :: Piece -> String
tooltip Queen = "Reine"
tooltip King = "Roi"
tooltip Rook = "Tour"
tooltip Bishop = "Fou"
tooltip Knight = "Cavalier"
tooltip _ = "Pièce personnalisée"

square :: ∀a. { piece :: Piece, capturable :: Boolean, selected :: Boolean, nonavailable :: Boolean} -> Array (Prop a) -> VDom a
square { piece, capturable, selected, nonavailable} props =
    div (props <> [
        class_ "queens-square",
        class' "queens-square-capturable" capturable,
        class' "queens-square-nonavailable" nonavailable,
        class' "queens-square-selected" selected
    ]) $ if piece == Empty then [] else [
        svg [width "100%", height "100%", class_ "queens-piece"] [
            use ("#piece-" <> show piece) [x_ "10%", y_ "10%", width "80%", height "80%"]
        ]
    ]

view :: State -> VDom Msg
view state = template {config, board, rules, customDialog, scoreDialog} state where
    position = state^._position
    rows = state^._nbRows
    columns = state^._nbColumns
        
    config = card "Les reines" [
        iconSizesGroup state [4∧4, 5∧5, 7∧7, 8∧8] true,
        iconSelectGroupM state "Pièces disponibles" piecesList (state^._allowedPieces) SelectAllowedPiece \piece ->
            _{icon = IconSymbol $ "#piece-" <> show piece, tooltip = Just $ tooltip piece},
        icongroup "Options" $ [
            iconbutton state
            {   icon: IconSymbol "#customize"
            ,   selected: N.head (state^._allowedPieces) == Custom
            ,   tooltip: Just "Crée ta propre propre pièce"
            }
            [ onclick Customize]
        ,
            iconbutton state
            {   icon: IconSymbol "#piece-mix"
            ,   selected: state^._multiPieces
            ,   tooltip: Just "Mode mixte"
            } [
                onclick ToggleMultiPieces
            ]
        ] <> [ihelp state, ireset state, irules state],
        iconBestScore state
    ]   

    pieceSelector = div [class_ "ui-flex-center gutter2 queens-pieceselector"] $
        N.toArray (state^._allowedPieces) <#> \piece ->
            let name = show piece in
            iconbutton state
            {   selected: piece == state^._selectedPiece
            ,   icon: IconSymbol $ "#piece-" <> name
            } 
            [   key name
            ,   onclick $ SelectPiece piece
            ]
        

    cursor pp = div ([class_ "ui-cursor"] <> cursorStyle pp rows columns 0.8) [
        svg [width "100%", height "100%"] [use ("#piece-" <> show (state^._selectedPiece)) []]
    ]

    grid = div ([class_ "ui-board"] <> gridStyle rows columns 5 <> trackPointer) $ concat [  
        map3 position (attackedBySelected state) (capturableSquares state) \index piece attacked capturable ->
            square {piece,
                selected: attacked || state^._selectedSquare == Just index,
                nonavailable: state^._help && (piece /= Empty || capturable),
                capturable
            } [
                style "width" $ pc (1.0 / toNumber columns),
                style "height" $ pc (1.0 / toNumber rows),
                onclick $ Play index,
                onpointerenter $ SelectSquare (Just index),
                onpointerleave $ SelectSquare Nothing
            ],
        [state^._pointer <??> cursor]
    ]

    board = div [] [
        pieceSelector,
        incDecGrid state [grid]
    ]

    angles = [45, 90, 135, 0, 0, 180, -45, -90, -135]

    customDialog _ = 
        dialog "Personnalise ta pièce"
        [   div [class_ "flex queens-custompiece"]
            [   div [class_ "queens-grid queens-custompiece-grid"] (
                    state^._customLocalMoves # mapWithIndex \index selected ->
                        square {
                                piece: if index == 12 then Custom else Empty,
                                selected: selected,
                                capturable: false,
                                nonavailable: false
                        } 
                        [   key $ show index 
                        ,   style "width" "20%"
                        ,   style "height" "20%"
                        ,   onclick' if index /= 12 then Just (FlipLocalMove index) else Nothing
                        ]
                )
            ,   div [class_ "flex queens-custompiece-directions"] (
                    map2 (state^._customDirections) angles \i selected angle ->
                        iconbutton state 
                        {   selected: selected
                        ,       icon: if i == 4 then IconNone else IconSymbol "#arrow"
                        ,   style: ["transform" ∧ ("rotate(" <> show angle <> "deg)")]
                        }
                        [   key $ show i
                        ,    onclick' $ if i /= 4 then Just (FlipDirection i) else Nothing
                        ]
            )
        ]
    ]    
        
    scoreDialog _ = bestScoreDialog state \pos -> [
        div [class_ "ui-flex-center queens-bestscore-container"] [
            div (gridStyle rows columns 5 <> [class_ "ui-board queens-grid"]) (
                pos <#> \piece ->
                    square { piece, capturable: false, selected: false, nonavailable: false} [
                        style "width" $ pc (1.0 / toNumber columns),
                        style "height" $ pc (1.0 / toNumber rows)
                    ]
            )
        ]
    ]

    rules = [
        text "Place le plus de pièces possible sur ta grille sans qu\'aucune ne soit menacée par une autre pièce.", br,
        text "Tu peux choisir de jouer avec différentes pièces comme celles du jeu d\'échecs."
    ]
    {-
            Fabrique ta propre pièce avec <Icon image="customize" />. Cette pièce est représentée par <Icon image="piece-custom" /><br />
            Le mode mixte permet de jouer avec plusieurs pièces.<br />
            Pour activer ce mode, clique sur <Icon image="piece-mix" /><br />
            Plusieurs tailles de tableau te sont proposées.<br />
            Là encore, tu peux personnaliser la taille du plateau avec <Icon text="NxM" /><br />
            Au cours de la partie, il peut être difficile de voir les cases libres (non menacées par des pièces posées).<br />
            Tu peux alors voir les cases libres avec l&#39;aide: <Icon image="help" />
    -}
