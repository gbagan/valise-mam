module Game.Noirblanc.View where

import Prelude
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Lens (Lens', (^.))
import Pha (Prop, VDom, text)
import Pha.Action ((🎲))
import Pha.Html (div', svguse, class', key, style, click)
import Lib.Util (coords, map2)
import Game.Core (_position, _nbRows, _nbColumns, _help, playA)
import Game.Noirblanc.Model (NoirblancState, _mode2, selectLevelA, selectModeA)
import UI.Icon (Icon(..), Options)
import UI.Icons (icongroup, ihelp, ireset, irules, iconSelectGroup)
import UI.Template (template, card, incDecGrid, gridStyle)

levels :: Int -> Options -> Options
levels i opt = case i of
    0 -> opt { icon = IconText "4x4" }
    1 -> opt { icon = IconText "4x4" }
    2 -> opt { icon = IconText "2x10" }
    3 -> opt { icon = IconText "3x10" }
    4 -> opt { icon = IconText "5x5" }
    5 -> opt { icon = IconText "NxM", tooltip = Just "Dimensions personnalisées" }
    _ -> opt { icon = IconSymbol "#lo-rand", tooltip = Just "Grille aléatoire" }

-- const lockedLevel = { symbol: 'locked', tooltip: 'Difficulté non débloquée', disabled: true };

square :: forall a. Boolean -> Boolean -> Array (Prop a) -> VDom a
square light cross props = 
    div' ([class' "noirblanc-square" true] <> props) [
        div' [class' "noirblanc-square-inner" true, class' "blanc" light] [
            div' [class' "noirblanc-square-blanc" true] $ 
                if cross then [svguse "#cross" [class' "ui-absolute noirblanc-cross" true]] else [],

            div' [class' "noirblanc-square-noir" true] $
                if cross then [svguse "#cross" [class' "ui-absolute noirblanc-cross" true]] else []
        ]
    ]

view :: forall a. Lens' a NoirblancState -> NoirblancState -> VDom a
view lens state = template lens {config, board, rules, winTitle} state where
    rows = state^._nbRows
    columns = state^._nbColumns
    position = state^._position
    grid = div' ([class' "ui-board" true] <> gridStyle rows columns 5) $
        map2 position.light position.played \index light played ->
            let {row, col} = coords columns index in
            square 
                light
                (state^._help && played) [
                key $ show index,
                style "height" $ show (86.0 / toNumber rows) <> "%",
                style "width" $ show (86.0 / toNumber columns) <> "%",
                style "left" $ show ((100.0 * toNumber col + 7.0) / toNumber columns) <> "%",
                style "top" $ show ((100.0 * toNumber row + 7.0) / toNumber rows) <> "%",
                click $ lens 🎲 playA index
            ]

    board = incDecGrid lens state [grid]

    config = card "Tout noir tout blanc" [
        let fn i = _{icon = IconSymbol $ "#lo-mode" <> show (i + 1)} in
        iconSelectGroup lens state "Mode jeu" [0, 1, 2, 3] fn (state^._mode2) selectModeA,
        let fn i opt = levels i opt in
        iconSelectGroup lens state "Difficulté" [0, 1, 2, 3, 4, 5, 6] fn (state^._mode2) selectLevelA,
        icongroup "Options" $ [ihelp, ireset, irules] <#> \x -> x lens state
    ]
    rules = [text "blablahblah"]

    winTitle = "GAGNÉ"