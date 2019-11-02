module Game.Chocolat.View where
  
import Prelude
import Data.Lens (Lens', (^.))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Game.Chocolat.Model (State, SoapMode(..), _soap, _soapMode, setSoapModeA) 
import Pha (VDom, text, emptyNode)
import Pha.Html (div', br)
import UI.Template (template, card)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, icons2Players, iconSelectGroup, iundo, iredo, ireset, irules)

{-
const inside = ({position}, row, col) =>
    col >= position.left && col <= position.right - 1 &&
    row >= position.top && row <= position.bottom - 1;

const inside2 = ({position}, row, col) =>
    col >= position.left && col <= position.right &&
    row >= position.top && row <= position.bottom &&    
    [position.left, position.right].includes(col)
    !== [position.top, position.bottom].includes(row);    
-}

view :: forall a. Lens' a State -> State -> VDom a
view lens state = template lens {config, board, rules, winTitle} state where
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
    {-
    grid = div' [] [
                -- hasDnD: true,
                -- class: 'ui-board',
                --style: gridStyle(state.rows, state.columns, 3)
        svg [width "100%", height "100%", viewBox "-7 -7 " <> show(50 * columns + 14) <> " " <> show(50 * rows + 14)][
            tabulate2 rows columns (row, col, i) =>
                rect (50.0 * toNumber col + 7) (50.0 * toNumber row + 7.0) 36.0 36.0 [
                    key ("choc" <> show i),
                    class' "chocolat-square" true,
                    class' "soap" (row == state.soap.row && col == state.soap.col),
                    class' "hidden" (not $ inside state row col))
                ],
            use (50.0 * toNumber soap.col + 12.0) (50.0 * toNumber state.soap.row + 12.0) 26.0 26.0 "#skull" [
                key "skull",
                fill "#20AF20"
            ] 
                    repeat2(state.rows+1, state.columns+1, (row, col, i) =>
                        inside2(state, row, col) && circle({
                            key: 'c' + i,
                            cx: 50 * col,
                            cy: 50 * row,
                            r: 7,
                            class: 'chocolat-cutter',
                            onpointerenter: [actions.showCutter, {row, col}],
                            onpointerleave: [actions.showCutter, null],
                            onclick: combine([actions.showCutter, null], [actions.play, {row, col}])
                        })
                    ),
                    state.cutter !== null && inside2(state, state.cutter.row, state.cutter.col) && line({
                        class: 'chocolat-line-to-pointer',
                        key: 'line',
                        x1: 50 * state.cutter.col,
                        y1: 50 * state.cutter.row,
                        x2: 50 * state.cutter2.col,
                        y2: 50 * state.cutter2.row
                    })
        ]
    ]
    -}
    board = emptyNode
            
    rules = [text "A chaque tour de ce jeu, tu peux déplacer une pile de jetons vers une case adjacente", br,
            text "qui contient au moins autant de jetons", br,
            text "Le but est de finir la partie avec le moins de cases contenant des piles de jetons."
    ]
    winTitle = "todo" -- winTitleFor2Players(state),
