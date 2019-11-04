module Game.Chocolat.View where
  
import MyPrelude
import Lib.Util (tabulate2)
import Game.Core (_position, _nbRows, _nbColumns, possibleMoves, playA)
import Game.Chocolat.Model (State, Move(..), SoapMode(..), _soap, _soapMode, setSoapModeA) 
import Pha (VDom, text)
import Pha.Action ((🎲))
import Pha.Html (div', svg, br, rect, circle, use, key, class', click, width, height, viewBox, fill)
import UI.Template (template, card, gridStyle, incDecGrid)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, icons2Players, iconSelectGroup, iundo, iredo, ireset, irules)

inside :: State -> Int -> Int -> Boolean
inside state row col = col >= left && col <= right - 1 && row >= top && row <= bottom - 1
    where {left, right, top, bottom} = state^._position
    
view :: forall a. Lens' a State -> State -> VDom a
view lens state = template lens {config, board, rules, winTitle} state where
    pos = state^._position
    rows = state^._nbRows
    columns = state^._nbColumns
    {col: soapCol, row: soapRow} = state^._soap

    config = card "Barre de chocolat" [
        iconSizesGroup lens state [Tuple 6 7] true,
        iconSelectGroup lens state "Emplacement du savon" [CornerMode, BorderMode, StandardMode] 
            (state^._soapMode) setSoapModeA \mode opt -> case mode of
                CornerMode -> opt{icon = IconSymbol "#choc-mode0", tooltip = Just "Dans le coin"}
                BorderMode -> opt{icon = IconSymbol "#choc-mode1", tooltip = Just "Sur un bord"}
                StandardMode -> opt{icon = IconSymbol "#choc-mode2", tooltip = Just "N'importe où"},
        icons2Players lens state,
        icongroup "Options" $ [iundo, iredo, ireset, irules] <#> \x -> x lens state
    ]

    cutter row col move = circle (50.0 * toNumber col) (50.0 * toNumber row) 7.0 [
        key $ "c" <> show (row * (columns + 1) + col), 
        class' "chocolat-cutter" true,
            --onpointerenter: [actions.showCutter, {row, col}],
            --onpointerleave: [actions.showCutter, null],
        click $ lens 🎲 playA move
            --    combine([actions.showCutter, null], [actions.play, {row, col}])
    ]

    grid = div' (gridStyle rows columns 3 <> [class' "ui-board" true]) [
        svg [width "100%", height "100%", viewBox (-7) (-7) (50 * columns + 14) (50 * rows + 14)] $
            concat [
                tabulate2 rows columns \row col ->
                    rect (50.0 * toNumber col + 7.0) (50.0 * toNumber row + 7.0) 36.0 36.0 [
                        key $ "choc" <> show (row * columns + col),
                        class' "chocolat-square" true,
                        class' "soap" (row == soapRow && col == soapCol),
                        class' "hidden" $ not (inside state row col)
                    ],
                [use (50.0 * toNumber soapCol + 12.0) (50.0 * toNumber soapRow + 12.0) 26.0 26.0 "#skull" [
                    key "skull",
                    fill "#20AF20"
                ]],
                possibleMoves state >>= case _ of
                    FromLeft i -> [cutter pos.top i (FromLeft i), cutter pos.bottom i (FromLeft i)]
                    FromRight i -> [cutter pos.top i (FromRight i), cutter pos.bottom i (FromRight i)]
                    FromTop i -> [cutter i pos.left (FromTop i), cutter i pos.right (FromTop i)]
                    FromBottom i -> [cutter i pos.left (FromBottom i), cutter i pos.right (FromBottom i)]
            ] {-
                    state.cutter !== null && inside2(state, state.cutter.row, state.cutter.col) && line({
                        class: 'chocolat-line-to-pointer',
                        key: 'line',
                        x1: 50 * state.cutter.col,
                        y1: 50 * state.cutter.row,
                        x2: 50 * state.cutter2.col,
                        y2: 50 * state.cutter2.row
                    })
                -}
    ]

    board = incDecGrid lens state [grid]
            
    rules = [text "A chaque tour de ce jeu, tu peux déplacer une pile de jetons vers une case adjacente", br,
            text "qui contient au moins autant de jetons", br,
            text "Le but est de finir la partie avec le moins de cases contenant des piles de jetons."
    ]
    winTitle = "todo" -- winTitleFor2Players(state),
