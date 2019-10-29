module Game.Queens.View where

import Prelude
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Lens (Lens', (^.))
import Data.Tuple (Tuple(..))
import Lib.Util (map3)
import Pha (h, text)
import Pha.Class (VDom, Prop)
import Pha.Action ((🎲))
import Pha.Html (div', br, class', attr, svg, use, style, width, height, href, click, pointerenter, pointerleave)
import Game.Core (_position, _nbRows, _nbColumns, _help, _pointerPosition, playA)
import Game.Queens.Model (QueensState, Piece(..), _selectedPiece, _selectedSquare, capturableSquares, attackedBySelected, selectSquareA)
import UI.Dialog (card)
import UI.Template (template, incDecGrid, gridStyle, trackPointer, cursorStyle)
import UI.Icons (iconSizesGroup)

{-
const pieceTooltips = {
    queen: 'Reine',
    king: 'Roi',
    rook: 'Tour',
    bishop: 'Fou',
    knight: 'Cavalier',
    custom: 'Pièce personnalisée'
};
-}

pieceToString :: Piece -> String
pieceToString Queen = "queen"
pieceToString King = "king" 
pieceToString Rook = "rook" 
pieceToString Bishop = "bishop"
pieceToString Knight = "knight"
pieceToString _ = "custom"

{-
    queen: 'Reine',
    king: 'Roi',
    rook: 'Tour',
    bishop: 'Fou',
    knight: 'Cavalier',
    custom: 'Pièce personnalisée'
};
-}


square :: forall a. { piece :: Piece, capturable :: Boolean, selected :: Boolean, nonavailable :: Boolean} -> Array (Prop a) -> VDom a
square { piece, capturable, selected, nonavailable} props =
    div' ([
        class' "queens-square" true,
        class' "queens-square-capturable" capturable,
        class' "queens-square-nonavailable" nonavailable,
        class' "queens-square-selected" selected
    ] <> props) $ if piece == Empty then [] else [
        svg [width "100%", height "100%", class' "queen-piece" true] [
            h "use" [href $ "#piece-" <> pieceToString piece, attr "x" "10%", attr "y" "10%",
                     width "80%", height "80%", class' "queens-piece" true] []
        ]
    ]

view :: forall a. Lens' a QueensState -> QueensState -> VDom a
view lens state = template lens {config, board, rules, winTitle} state where
    position = state^._position
    rows = state^._nbRows
    columns = state^._nbColumns
        
    config = card "Les reines" [
        iconSizesGroup lens state [Tuple 4 4, Tuple 5 5, Tuple 7 7, Tuple 8 8] true
    ]

    {-

            I.Group({
                title: 'Pièces disponibles',
                list: piecesList,
                multi: true,
                select: state.allowedPieces,
                onclick: actions.selectAllowedPiece,
                symbol: 'piece-',
                tooltip: pieceTooltips
            }),

            I.Group({ title: 'Options' },
                I.Icon({
                    symbol: 'customize',
                    tooltip: 'Crée ta propre propre pièce',
                    selected: state.allowedPieces[0] === 'custom',
                    onclick: actions.customize,
                }),
                I.Icon({
                    symbol: 'piece-mix',
                    selected: state.multiPieces,
                    tooltip: 'Mode mixte',
                    onclick: actions.toggleMultiPieces,
                }),
                I.Help(),
                I.Rules()
            ),

            I.Group({ title: `Meilleur score (${state.bestScore || 0})` },
                I.BestScore()
            )
        )
    );
    ]

    const PieceSelector = () =>
        div({ class: 'ui-flex-center gutter2 queens-pieceselector' },
            state.allowedPieces.map(piece =>
                Icon({
                    key: piece,
                    selected: piece === state.selectedPiece,
                    symbol: 'piece-' + piece,
                    onclick: [actions.selectPieceType, piece],
                })
            )
        );

    -}

    cursor pp = div' ([class' "ui-cursor" true] <> cursorStyle pp rows columns 80.0) [
        svg [width "100%", height "100%"] [
            h "use" [href $ "#piece-" <> pieceToString (state^._selectedPiece)] []
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
        )     
                   -- capturable: state.attackedSquares[index],
                   -- nonavailable: state.help && (!!piece || state.attackedSquares[index]),
                   -- selected: state.selectedSquare !== null &&
                   --          (state.attackedBySelected[index] || index === state.selectedSquare),
        <> (state^._pointerPosition # maybe [] (pure <<< cursor))

    board =  div' [] [
           --  PieceSelector(),
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