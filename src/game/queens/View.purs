module Game.Queens.View where

import Prelude
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Lens (Lens', (^.))
import Data.Tuple (Tuple(..))
import Data.Array (head)
import Lib.Util (map3)
import Pha (h, text)
import Pha.Class (VDom, Prop)
import Pha.Action ((🎲))
import Pha.Html (div', br, class', attr, svg, key, style, width, height, href, click, pointerenter, pointerleave)
import Game.Core (_position, _nbRows, _nbColumns, _help, _pointerPosition)
import Game.Queens.Model (QueensState, Piece(..), _selectedPiece, _selectedSquare, _allowedPieces, _multiPieces,
                           piecesList, capturableSquares, attackedBySelected,
                           playA, selectSquareA, selectPieceA, selectAllowedPieceA, toggleMultiPiecesA)
import UI.Template (template, card, incDecGrid, gridStyle, trackPointer, cursorStyle)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSizesGroup, iconSelectGroupM, ihelp, irules, ireset)

tooltip :: Piece -> String
tooltip Queen = "Reine"
tooltip King = "Roi"
tooltip Rook = "Tour"
tooltip Bishop = "Fou"
tooltip Knight = "Cavalier"
tooltip _ = "Pièce personnalisée"

square :: forall a. { piece :: Piece, capturable :: Boolean, selected :: Boolean, nonavailable :: Boolean} -> Array (Prop a) -> VDom a
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

view :: forall a. Lens' a QueensState -> QueensState -> VDom a
view lens state = template lens {config, board, rules, winTitle} state where
    position = state^._position
    rows = state^._nbRows
    columns = state^._nbColumns
        
    config = card "Les reines" [
        iconSizesGroup lens state [Tuple 4 4, Tuple 5 5, Tuple 7 7, Tuple 8 8] true,
        iconSelectGroupM lens state "Pièces disponibles" piecesList 
                (\piece -> _{icon = IconSymbol $ "#piece-" <> show piece, tooltip = Just $ tooltip piece}) 
                (state^._allowedPieces) selectAllowedPieceA,
        icongroup "Options" $ [
            iconbutton state (_{icon = IconSymbol "#customize",
                              selected = head (state^._allowedPieces) == Just Custom,
                              tooltip = Just "Crée ta propre propre pièce"}) [],
            iconbutton state (_{icon = IconSymbol "#piece-mix", selected = state^._multiPieces, tooltip = Just "Mode mixte"}) [
                click $ lens 🎲 toggleMultiPiecesA
            ]
        ] <> ([ihelp, ireset, irules] <#> \x -> x lens state)
    ]
            {-    
            I.Group({ title: `Meilleur score (${state.bestScore || 0})` },
                I.BestScore()

    -}
    pieceSelector = div' [class' "ui-flex-center gutter2 queens-pieceselector" true] $
        state^._allowedPieces <#> \piece ->
            let name = show piece in
            iconbutton state (\x -> x{
                    selected = piece == state^._selectedPiece,
                    icon = IconSymbol $ "#piece-" <> name
                }) [
                    key $ name,
                    click $ lens 🎲 selectPieceA piece
                ]
        

    cursor pp = div' ([class' "ui-cursor" true] <> cursorStyle pp rows columns 80.0) [
        svg [width "100%", height "100%"] [
            h "use" [href $ "#piece-" <> show (state^._selectedPiece)] []
        ]
    ]

    grid = div' ([class' "ui-board" true] <> gridStyle rows columns <> trackPointer lens) $ -- todo ui-board? (dnd)     
        (map3 position (attackedBySelected state) (capturableSquares state) \index piece attacked capturable ->
            square {piece,
                selected: attacked || state^._selectedSquare == Just index,
                nonavailable: state^._help && (piece /= Empty || capturable),
                capturable
            } [
                style "width" $ show (100.0 / toNumber columns) <> "%",
                style "height" $ show (100.0 / toNumber rows) <> "%",
                click $ lens 🎲 playA index,
                pointerenter $ lens 🎲 selectSquareA (Just index),
                pointerleave $ lens 🎲 selectSquareA Nothing
            ]
        ) <> (state^._pointerPosition # maybe [] (pure <<< cursor))

    board = div' [] [
        pieceSelector,
        incDecGrid lens state [grid]
    ]

    directions = [1, 2, 3, 0, 0, 4, 7, 6, 5]

    {-
    const CustomPieceDialog = () =>
        Dialog({
            onOk: [actions.showDialog, null],
            title: 'Pièce personnalisée'
        },
            div({class: 'flex'},
                div({class: 'queens-custompiece-grid'},
                    div({
                        class: 'queens-grid',
                        style: gridStyle(state.rows, state.columns)
                    },
                        state.customMoves.local.map((selected, index) =>
                            Square({
                                style: { width: '20%', height: '20%' },
                                key: index,
                                piece: index === 12 && 'custom',
                                selected,
                                onclick: index !== 12 && [actions.flipLocal, index],
                            })
                        )
                    )
                ),
                div ({class: 'flex queens-custompiece-directions'},
                    state.customMoves.directions.map((selected, i) =>
                        Icon({
                            key: i,
                            selected,
                            symbol: i != 4 ? 'arrow' : null,
                            style: {transform: `rotate(${45 * directions[i]}deg)`},
                            onclick: i != 4 && [actions.flipDirection, i]
                        })
                    )
                )
            )
        );

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


    const dialogs = {
        bestscore: BestScoreDialog,
        custompiece: CustomPieceDialog,
    };
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