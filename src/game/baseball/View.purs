module Game.Baseball.View (view) where

import MyPrelude
import Lib.Util (map2)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc, translate)
import Game.Core (canPlay, isLevelFinished, _position)
import Game.Baseball.Model (State, Msg(..), _nbBases, _missingPeg)
import UI.Template (template, card)
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)

colors ∷ Array String
colors = ["blue", "red", "green", "magenta", "orange", "black", "cyan", "gray"]
dupColors ∷ Array String
dupColors = colors >>= \x → [x, x]

translatePeg ∷ Int → Int → String
translatePeg position nbBases = translate (pc x) (pc y) where
    mid = toNumber (position / 2)
    x = 0.42 + 0.35 * cos (mid * 2.0 * pi / toNumber nbBases) + 0.1 * toNumber (position `mod` 2)
    y = 0.46 + 0.35 * sin (mid * 2.0 * pi / toNumber nbBases)

transformBase ∷ Int → Int → String
transformBase i nbBases = translate (pc x) (pc y)  <> " rotate(45deg)" where
    x = 0.50 + 0.35 * cos (toNumber i * 2.0 * pi / toNumber nbBases)
    y = 0.50 + 0.35 * sin (toNumber i * 2.0 * pi / toNumber nbBases)

view ∷ State → Html Msg
view state = template {config, board, rules} state where
    position = state ^. _position
    nbBases = state ^. _nbBases
    levelFinished = isLevelFinished state
    missingPeg = state ^. _missingPeg

    config =
        card "Baseball multicolore"
        [   iconSelectGroup state "Nombres de bases" [4, 5, 6, 7, 8] nbBases SetNbBases (const identity)
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> (_ $ state)
        ]

    board = H.div [H.class_ "ui-board baseball-board"] [
        H.svg [P.viewBox 0 0 100 100]
        [   H.g [] $
                take nbBases colors # mapWithIndex \i color →
                    H.rect
                    [   H.class_ "baseball-base"
                    ,   P.stroke color
                    ,   H.style "transform" $ transformBase i nbBases
                    ]
        ,   H.g [] $
                map2 position dupColors \peg pos color →
                    H.when (peg ≠ missingPeg) \_ →
                        H.g
                        [   H.class_ "baseball-player"
                        ,   H.style "transform" $ translatePeg pos nbBases
                        ]
                        [   H.use 
                            [   P.href "#meeple"
                            ,   P.width "7"
                            ,   P.height "7"
                            ,   E.onclick $ Play peg
                            ,   P.fill color
                            ,   H.style "animation"
                                    if levelFinished then
                                        "baseballHola 4s linear " <> show (1000 + 2000 * peg / nbBases) <> "ms infinite"
                                    else
                                        "none"
                            ,   H.style "cursor" (if canPlay state peg then "pointer" else "not-allowed")
                            ]
                    ]
        ]
    ]

    rules =
        [   H.text "Le but du jeu est d'amener chaque jeton dans sa base (celle qui a la même couleur que le jeton)."
        ,   H.br
        ,   H.text "Pour cela, tu peux déplacer un jeton vers une base adjacente si celle-ci possède un emplacement libre."
        ,   H.br
        ,   H.text "Pour déplacer un jeton, il te suffit de cliquer dessus."
        ]