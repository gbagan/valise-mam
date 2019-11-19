module Game.Chocolat.View  (view) where
  
import MyPrelude
import Lib.Util (tabulate2)
import Game.Effs (EFFS)
import Game.Core (_position, _nbRows, _nbColumns, possibleMoves, playA)
import Game.Chocolat.Model (State, Move(..), SoapMode(..), _soap, _soapMode, _moveWhenHover, cutLine, setSoapModeA, setHoverA) 
import Pha (VDom, text, maybeN)
import Pha.Action ((🔍))
import Pha.Html (div', span, svg, br, rect, line, circle, use, key, class', click, pointerenter, pointerleave, viewBox, fill)
import UI.Template (template, card, gridStyle, incDecGrid, turnMessage, winTitleFor2Players)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, icons2Players, iconSelectGroup, iundo, iredo, ireset, irules)

inside :: State -> Int -> Int -> Boolean
inside state row col = col >= left && col <= right - 1 && row >= top && row <= bottom - 1
    where {left, right, top, bottom} = state^._position
    
view :: ∀a. Lens' a State -> State -> VDom a EFFS
view lens state = template lens _{config=config, board=board, rules=rules, winTitle=winTitle} state where
    pos = state^._position
    rows = state^._nbRows
    columns = state^._nbColumns
    {col: soapCol, row: soapRow} = state^._soap

    config = card "Barre de chocolat" [
        iconSizesGroup lens state [6∧7] true,
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
        pointerenter $ lens 🔍 setHoverA (Just move),
        pointerleave $ lens 🔍 setHoverA Nothing,
        click $ lens 🔍 (setHoverA Nothing *> playA move)
    ]

    grid = div' (gridStyle rows columns 3 <> [class' "ui-board" true]) [
        svg [viewBox (-7) (-7) (50 * columns + 14) (50 * rows + 14)] (
            concat [
                tabulate2 rows columns \row col ->
                    rect (50.0 * toNumber col + 7.0) (50.0 * toNumber row + 7.0) 36.0 36.0 [
                        key $ "choc" <> show (row * columns + col),
                        class' "chocolat-square" true,
                        class' "soap" (row == soapRow && col == soapCol),
                        class' "hidden" $ not (inside state row col)
                    ],
                possibleMoves state >>= case _ of
                    FromLeft i -> [cutter pos.top i (FromLeft i), cutter pos.bottom i (FromLeft i)]
                    FromRight i -> [cutter pos.top i (FromRight i), cutter pos.bottom i (FromRight i)]
                    FromTop i -> [cutter i pos.left (FromTop i), cutter i pos.right (FromTop i)]
                    FromBottom i -> [cutter i pos.left (FromBottom i), cutter i pos.right (FromBottom i)],
                [
                    use (50.0 * toNumber soapCol + 12.0) (50.0 * toNumber soapRow + 12.0) 26.0 26.0 "#skull" [
                        key "skull",
                        fill "#20AF20"
                    ],
                    maybeN $ state^._moveWhenHover <#> \m ->
                        let {x1, x2, y1, y2} = cutLine state m
                        in line (50 * x1) (50 * y1) (50 * x2) (50 * y2) [key "line", class' "chocolat-line-to-pointer" true]
                ]
            ]
        )
    ]

    board = incDecGrid lens state [
        grid,
        span [class' "frog-turn-message" true] [text (turnMessage state)]
    ]
            
    rules = [text "A chaque tour de ce jeu, tu peux déplacer une pile de jetons vers une case adjacente", br,
            text "qui contient au moins autant de jetons", br,
            text "Le but est de finir la partie avec le moins de cases contenant des piles de jetons."
    ]
    winTitle = winTitleFor2Players state
