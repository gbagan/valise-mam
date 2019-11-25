module Game.Noirblanc.View where

import MyPrelude
import Lib.Util (coords, map2)
import Pha (Prop, VDom, text, class_, class', key, style)
import Pha.Elements (div)
import Pha.Attributes (onclick)
import Pha.Svg (svg, use, width, height)
import Pha.Util (pc)
import Game.Core (_position, _nbRows, _nbColumns, _help)
import Game.Effs (EFFS)
import Game.Noirblanc.Model (State, _level, _mode, _maxLevels, play2A, selectLevelA, selectModeA)
import UI.Icon (Icon(..), Options)
import UI.Icons (icongroup, ihelp, ireset, irules, iconSelectGroup)
import UI.Template (template, card, incDecGrid, gridStyle)

levelOptions :: Int -> Boolean -> Options -> Options
levelOptions _ true opt = opt{icon = IconSymbol "#locked", tooltip = Just "Difficulté non débloquée", disabled = true}
levelOptions level _ opt = case level of
    0 -> opt{ icon = IconText "3x3" }
    1 -> opt{ icon = IconText "4x4" }
    2 -> opt{ icon = IconText "2x10" }
    3 -> opt{ icon = IconText "3x10" }
    4 -> opt{ icon = IconText "5x5" }
    5 -> opt{ icon = IconText "NxM", tooltip = Just "Dimensions personnalisées" }
    _ -> opt{ icon = IconSymbol "#lo-rand", tooltip = Just "Grille aléatoire" }

square :: ∀a. Boolean -> Boolean -> Array (Prop a EFFS) -> VDom a EFFS
square light cross props = 
    div ([class_ "noirblanc-square"] <> props) [
        div [class_ "noirblanc-square-inner", class' "blanc" light] [
            div [class_ "noirblanc-square-blanc"] $ 
                if cross then [
                    svg [width "100%", height "100%", class_ "ui-absolute noirblanc-cross"] [use "#cross" []]
                ]  else [],

            div [class_ "noirblanc-square-noir"] $
                if cross then [
                    svg [width "100%", height "100%", class_ "ui-absolute noirblanc-cross"] [use "#cross" []]
                ] else []
        ]
    ]

view :: State -> VDom State EFFS
view state = template _{config=config, board=board, rules=rules, winTitle=winTitle} state where
    rows = state^._nbRows
    columns = state^._nbColumns
    position = state^._position

    config = card "Tout noir tout blanc" [
        iconSelectGroup state "Mode jeu" [0, 1, 2, 3] (state^._mode) selectModeA \i ->
            _{icon = IconSymbol $ "#lo-mode" <> show (i + 1)},
        iconSelectGroup state "Difficulté" [0, 1, 2, 3, 4, 5, 6] (state^._level) selectLevelA \i ->
            levelOptions i (Just i > (state^._maxLevels) !! (state^._mode)),
        icongroup "Options" $ [ihelp state, ireset state, irules state]
    ]

    grid = div ([class' "ui-board" true] <> gridStyle rows columns 4) $
        map2 position.light position.played \index light played ->
            let {row, col} = coords columns index in
            square light (state^._help && played) [
                key $ show index,
                style "height" $ pc (0.86 / toNumber rows),
                style "width" $ pc (0.86 / toNumber columns),
                style "left" $ pc ((toNumber col + 0.07) / toNumber columns),
                style "top" $ pc ((toNumber row + 0.07) / toNumber rows),
                onclick $ play2A index
            ]

    board = incDecGrid state [grid]

    rules = [text "blablahblah"]

    winTitle = "GAGNÉ"