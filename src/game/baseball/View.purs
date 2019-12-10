module Game.Baseball.View (view) where

import MyPrelude
import Lib.Util (map2)
import Pha (VDom, text, (<&&>), class_, key, style)
import Pha.Elements (div, br)
import Pha.Events (onclick)
import Pha.Svg (svg, g, rect, use, stroke, fill, viewBox, x_, y_, width, height)
import Pha.Util (pc, translate)
import Game.Core (canPlay, isLevelFinished, _position)
import Game.Baseball.Model (State, Msg(..), _nbBases, _missingPeg)
import UI.Template (template, card)
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)

colors ∷ Array String
colors = ["blue", "red", "green", "magenta", "orange", "black", "cyan", "gray"]
dupColors ∷ Array String
dupColors = colors >>= \x → [x, x]

translatePeg ∷ Int → Int → String
translatePeg position nbBases = translate (pc x) (pc y)  where
    mid = toNumber (position / 2)
    x = 0.42 + 0.35 * cos (mid * 2.0 * pi / toNumber nbBases) + 0.1 * toNumber (position `mod` 2)
    y = 0.46 + 0.35 * sin (mid * 2.0 * pi / toNumber nbBases)

transformBase ∷ Int → Int → String
transformBase i nbBases = translate (pc x) (pc y)  <> " rotate(45deg)" where
    x = 0.50 + 0.35 * cos (toNumber i * 2.0 * pi / toNumber nbBases)
    y = 0.50 + 0.35 * sin (toNumber i * 2.0 * pi / toNumber nbBases)

view ∷ State → VDom Msg
view state = template {config, board, rules} state where
    nbBases = state^._nbBases
    levelFinished = isLevelFinished state

    config = card "Baseball multicolore" [
        iconSelectGroup state "Nombres de bases" [4, 5, 6, 7, 8] nbBases SetNbBases (const identity),
        icongroup "Options" $ [iundo state, iredo state, ireset state, irules state]
    ]

    board = div [class_ "ui-board baseball-board"] [
        svg [viewBox 0 0 100 100] $ concat
        [   take nbBases colors # mapWithIndex \i color →
                rect
                [   key $ "b" <> show i
                ,   x_ "-10"
                ,   y_ "-10"
                ,   width "20"
                ,   height "20"
                ,   class_ "baseball-base"
                ,   stroke color
                ,   style "transform" $ transformBase i nbBases
                ]
        ,   map2 (state^._position) dupColors \peg pos color →
                peg /= state^._missingPeg <&&> \_ →
                    g
                    [   class_ "baseball-player"
                    ,   style "transform" $ translatePeg pos nbBases
                    ,   key $ "p" <> show peg
                    ]
                    [   use "#meeple"
                        [   width "7"
                        ,   height "7"
                        ,   onclick $ Play peg
                        ,   fill color
                        ,   style "animation"
                                if levelFinished then
                                    "baseballHola 4s linear " <> show (1000 + 2000 * peg / nbBases) <> "ms infinite"
                                else
                                    "none"
                        ,   style "cursor" (if canPlay state peg then "pointer" else "not-allowed")
                        ]
                    ]
        ]
    ]

    rules =
        [   text "Le but du jeu est d'amener chaque jeton dans sa base (celle qui a la même couleur que le jeton)."
        ,   br
        ,   text "Pour cela, tu peux déplacer un jeton vers une base adjacente si celle-ci possède un emplacement libre."
        ,   br
        ,   text "Pour déplacer un jeton, il te suffit de cliquer dessus."
        ]
    