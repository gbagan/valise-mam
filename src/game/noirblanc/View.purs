module Game.Noirblanc.View where

import MyPrelude

import Game.Core (_position, _nbRows, _nbColumns, _help)
import Game.Noirblanc.Model (State, Msg(..), _level, _mode, _maxLevels)
import Lib.Util (coords, map2)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc)
import UI.Icon (Icon(..), Options)
import UI.Icons (icongroup, ihelp, ireset, irules, iconSelectGroup)
import UI.Template (template, card, incDecGrid, gridStyle)

levelOptions ∷ Int → Boolean → Record Options → Record Options
levelOptions _ true opt = opt{icon = IconSymbol "#locked", tooltip = Just "Difficulté non débloquée", disabled = true}
levelOptions level _ opt = case level of
    0 → opt{ icon = IconText "3x3" }
    1 → opt{ icon = IconText "4x4" }
    2 → opt{ icon = IconText "2x10" }
    3 → opt{ icon = IconText "3x10" }
    4 → opt{ icon = IconText "5x5" }
    5 → opt{ icon = IconText "NxM", tooltip = Just "Dimensions personnalisées" }
    _ → opt{ icon = IconSymbol "#lo-rand", tooltip = Just "Grille aléatoire" }

square ∷ ∀a. Boolean → Boolean → Array (H.Prop a) → Html a
square light cross props = 
    H.div ([H.class_ "noirblanc-square"] <> props)
    [   H.div [H.class_ "noirblanc-square-inner", H.class' "blanc" light]
        [   H.div [H.class_ "noirblanc-square-blanc"] $ 
                if cross then [
                    H.svg [H.class_ "ui-absolute noirblanc-cross"] [H.use [P.href "#cross"]]
                ]  else []
        ,   H.div [H.class_ "noirblanc-square-noir"] $
                if cross then [
                    H.svg [H.class_ "ui-absolute noirblanc-cross"] [H.use [P.href "#cross"]]
                ] else []
        ]
    ]

view ∷ State → Html Msg
view state = template {config, board, rules} state where
    rows = state ^. _nbRows
    columns = state ^. _nbColumns
    position = state ^. _position
    level = state ^. _level
    mode = state ^. _mode
    maxLevels = state ^. _maxLevels
    help = state^._help

    config = card "Tout noir tout blanc" [
        iconSelectGroup state "Mode jeu" [0, 1, 2, 3] mode SelectMode \i →
            _{icon = IconSymbol $ "#lo-mode" <> show (i + 1)},
        iconSelectGroup state "Difficulté" [0, 1, 2, 3, 4, 5, 6] level SelectLevel \i →
            levelOptions i (Just i > maxLevels !! mode),
        icongroup "Options" $ [ihelp, ireset, irules] <#> (_ $ state)
    ]

    grid = H.div ([H.class_ "ui-board"] <> gridStyle rows columns 4) $
        map2 position.light position.played \index light played →
            let {row, col} = coords columns index in
            square light (help && played)
            [   H.style "height" $ pc (0.86 / toNumber rows)
            ,   H.style "width" $ pc (0.86 / toNumber columns)
            ,   H.style "left" $ pc ((toNumber col + 0.07) / toNumber columns)
            ,   H.style "top" $ pc ((toNumber row + 0.07) / toNumber rows)
            ,   E.onClick \_ → Play index
            ]

    board = incDecGrid state [grid]

    rules =
        [   H.text "Le but du jeu est de retourner des tuiles pour que toutes soient face noire."
        ,   H.br
        ,   H.text "La difficulté est que lorsque tu retournes une tuile, les tuiles adjacentes sont également retournées."
        ,   H.br
        ,   H.text "Ce jeu possède différents niveaux débloqués au fur et à mesure ainsi que d'autres modes de jeu. Selon le mode choisi, les règles pour retourner les tuiles changent."
        ]
