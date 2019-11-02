module Game.Jetons.View where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Lens (Lens', (^.))
import Data.Int (floor, toNumber)
import Data.Array (filter, length, mapWithIndex)
import Math (sqrt)
import Pha (VDom, text, whenN, maybeN)
import Pha.Html (div', span, br, key, class', style, rgbColor)
import Game.Core (_position, _nbColumns, _nbRows, _pointerPosition)
import Game.Jetons.Model (State, _dragged)
import Lib.Util (coords)
import UI.Template (template, card, incDecGrid, gridStyle, dndBoardProps, dndItemProps, cursorStyle)
import UI.Icons (icongroup, iconSizesGroup, iundo, iredo, ireset, irules)

view :: forall a. Lens' a State -> State -> VDom a
view lens state = template lens {config, board, rules, winTitle} state where
    position = state^._position
    columns = state^._nbColumns
    rows = state^._nbRows

    config = card "Jeu des jetons" [
        iconSizesGroup lens state [Tuple 2 2, Tuple 4 4, Tuple 5 5, Tuple 6 6] true,
        icongroup "Options" $ [iundo, iredo, ireset, irules] <#> \x -> x lens state
        --    I.Group({title: `Meilleur score (${state.bestScore || '∅'})`},
        --        I.BestScore()
    
    ]

    cursor pp _ = div' ([class' "ui-cursor jetons-cursor" true] <> cursorStyle pp rows columns 60.0) []

    piece i val props =
        let {row, col} = coords columns i in
        div' ([
            key $ show i,
            class' "jetons-peg" true,
            class' "small" $ columns >= 8,
            style "background-color" $ rgbColor 255 (floor $ 255.0 * (1.0 - sqrt (toNumber val / toNumber (rows * columns)))) 0,
            style "left" $ show ((15.0 + toNumber col * 100.0) / toNumber columns) <> "%",
            style "top" $ show ((15.0 + toNumber row * 100.0) / toNumber rows) <> "%",
            style "width" $ show (70.0 / toNumber columns) <> "%",
            style "height" $ show (70.0 / toNumber rows) <> "%",
            style "box-shadow" $ show (val * 2) <> "px " <> show(val * 2) <> "px 5px 0px #656565"
        ] <> props) [ span [] [text $ show val] ]
        {-
        const BestScoreDialog = () =>
        Dialog({
            title: 'Meilleur score',
            onOk: [actions.showDialog, null]
        },
            div({class: 'ui-flex-center jetons-bestscore-grid-container'}, 
                div({
                    class: 'ui-board',
                    style: gridStyle(state.rows, state.columns, 3)
                },
                    state.bestPosition.map((val, i) => val !== 0 &&
                        Piece({ key: i, index: i, val })
                    )
                )
            )
        ); -}

    board = incDecGrid lens state [
        div' ([class' "ui-board" true] <> dndBoardProps lens _dragged <> gridStyle rows columns 3) $
            (position # mapWithIndex \i val ->
                whenN (val /= 0) \_ ->
                    piece i val ([key $ show i] <> dndItemProps lens _dragged true true i state)
            ) <> [maybeN $ cursor <$> state^._pointerPosition <*> state^._dragged]
    ]

    rules = [
        text "A chaque tour de ce jeu, tu peux déplacer une pile de jetons vers une case adjacente", br,
        text "qui contient au moins autant de jetons", br,
        text "Le but est de finir la partie avec le moins de cases contenant des piles de jetons."
    ]

    nbNonEmptyCells = position # filter (_ > 0) # length
    s = if nbNonEmptyCells > 1 then "s" else ""

    winTitle = show nbNonEmptyCells <> " case" <> s <> " restante" <> s