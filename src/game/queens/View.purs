module Game.Queens.View where

import MyPrelude
import Lib.Util (map2, map3)
import Data.Array.NonEmpty (toArray, head) as N
import Pha (VDom, Prop, h, text)
import Pha.Action ((🔍))
import Pha.Html (div', br, class', attr, svg, key, style, pc, width, height, href, click, pointerenter, pointerleave)
import Game.Effs (EFFS)
import Game.Core (_position, _nbRows, _nbColumns, _help, _pointer)
import Game.Queens.Model (State, Piece(..),
                           _selectedPiece, _selectedSquare, _allowedPieces, _multiPieces, _customLocalMoves, _customDirections,
                           piecesList, capturableSquares, attackedBySelected,
                 customizeA,  playA, selectSquareA, selectPieceA, selectAllowedPieceA, toggleMultiPiecesA, flipLocalMoveA, flipDirectionA)
import UI.Template (template, card, dialog, incDecGrid, gridStyle, trackPointer, cursorStyle)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSizesGroup, iconSelectGroupM, ihelp, irules, ireset)

tooltip :: Piece -> String
tooltip Queen = "Reine"
tooltip King = "Roi"
tooltip Rook = "Tour"
tooltip Bishop = "Fou"
tooltip Knight = "Cavalier"
tooltip _ = "Pièce personnalisée"

square :: ∀a. { piece :: Piece, capturable :: Boolean, selected :: Boolean, nonavailable :: Boolean} -> Array (Prop a EFFS) -> VDom a EFFS
square { piece, capturable, selected, nonavailable} props =
    div' ([
        class' "queens-square" true,
        class' "queens-square-capturable" capturable,
        class' "queens-square-nonavailable" nonavailable,
        class' "queens-square-selected" selected
    ] <> props) $ if piece == Empty then [] else [
        svg [width "100%", height "100%", class' "queen-piece" true] [
            h "use" [href $ "#piece-" <> show piece, attr "x" "10%", attr "y" "10%",
                     width "80%", height "80%", class' "queens-piece" true] []
        ]
    ]

view :: ∀a. Lens' a State -> State -> VDom a EFFS
view lens state = template lens (_{config=config, board=board, rules=rules, winTitle=winTitle, customDialog = customDialog}) state where
    position = state^._position
    rows = state^._nbRows
    columns = state^._nbColumns
        
    config = card "Les reines" [
        iconSizesGroup lens state [4~4, 5~5, 7~7, 8~8] true,
        iconSelectGroupM lens state "Pièces disponibles" piecesList (state^._allowedPieces) selectAllowedPieceA \piece opt ->
            opt{icon = IconSymbol $ "#piece-" <> show piece, tooltip = Just $ tooltip piece},
        icongroup "Options" $ [
            iconbutton state (_{icon = IconSymbol "#customize",
                              selected = N.head (state^._allowedPieces) == Custom,
                              tooltip = Just "Crée ta propre propre pièce"})[
                                  click $ lens 🔍 customizeA
                              ],
            iconbutton state (_{icon = IconSymbol "#piece-mix", selected = state^._multiPieces, tooltip = Just "Mode mixte"}) [
                click $ lens 🔍 toggleMultiPiecesA
            ]
        ] <> ([ihelp, ireset, irules] <#> \x -> x lens state)
    ]
            {-    
            I.Group({ title: `Meilleur score (${state.bestScore || 0})` },
                I.BestScore()

    -}
    pieceSelector = div' [class' "ui-flex-center gutter2 queens-pieceselector" true] $
        N.toArray (state^._allowedPieces) <#> \piece ->
            let name = show piece in
            iconbutton state (\x -> x{
                    selected = piece == state^._selectedPiece,
                    icon = IconSymbol $ "#piece-" <> name
                }) [
                    key $ name,
                    click $ lens 🔍 selectPieceA piece
                ]
        

    cursor pp = div' ([class' "ui-cursor" true] <> cursorStyle pp rows columns 80.0) [
        svg [width "100%", height "100%"] [
            h "use" [href $ "#piece-" <> show (state^._selectedPiece)] []
        ]
    ]

    grid = div' ([class' "ui-board" true] <> gridStyle rows columns 5 <> trackPointer lens) $   
        (map3 position (attackedBySelected state) (capturableSquares state) \index piece attacked capturable ->
            square {piece,
                selected: attacked || state^._selectedSquare == Just index,
                nonavailable: state^._help && (piece /= Empty || capturable),
                capturable
            } [
                style "width" $ pc (100.0 / toNumber columns),
                style "height" $ pc (100.0 / toNumber rows),
                click $ lens 🔍 playA index,
                pointerenter $ lens 🔍 selectSquareA (Just index),
                pointerleave $ lens 🔍 selectSquareA Nothing
            ]
        ) <> (state^._pointer # maybe [] (pure ∘ cursor))

    board = div' [] [
        pieceSelector,
        incDecGrid lens state [grid]
    ]

    angles = [45, 90, 135, 0, 0, 180, -45, -90, -135]

    customDialog = dialog lens "Personnalise ta pièce" [
        div' [class' "flex queens-custompiece" true] [
            div' [class' "queens-grid queens-custompiece-grid" true] (
                state^._customLocalMoves # mapWithIndex \index selected ->
                    square {
                            piece: if index == 12 then Custom else Empty,
                            selected: selected,
                            capturable: false,
                            nonavailable: false
                    } [key $ show index, 
                        style "width" "20%", style "height" "20%",
                        click if index /= 12 then lens 🔍 flipLocalMoveA index else pure unit
                    ]
            ),
            div' [class' "flex queens-custompiece-directions" true] (
                map2 (state^._customDirections) angles \i selected angle ->
                    iconbutton state (_{
                        selected = selected,
                        icon = if i == 4 then IconNone else IconSymbol "#arrow",
                        style = ["transform" ~ ("rotate(" <> show angle <> "deg)")]
                    }) [
                        key $ show i,
                        click $ if i /= 4 then lens 🔍 flipDirectionA i else pure unit
                    ]
            )
        ]
    ]    
        
   {-
    const BestScoreDialog = () =>
        Dialog({
            title: 'Meilleur score',
            onOk: [actions.showDialog, null]
        },
            div({ class: 'ui-flex-center queens-bestscore-container' },
                div({
                    class: 'queens-grid',
                    style: gridStyle(state.rows, state.columns)
                },
                    state.bestPosition.map(piece =>
                        Square({
                            style: {
                                width: 100 / state.columns + '%',
                                height: 100 / state.rows + '%'
                            },
                            piece,
                        })
                    )
                )
            )
        );
    -}

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

    winTitle = ""