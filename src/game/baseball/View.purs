module Game.Baseball.View (view) where

import MyPrelude
import Lib.Util (map2)
import Pha (VDom, text, ifN, class', key, style)
import Pha.Elements (div)
import Pha.Attributes (onclick)
import Pha.Svg (svg, g, rect, use, stroke, fill, viewBox, x_, y_, width, height)
import Pha.Util (pc, translate)
import Game.Core (canPlay, core, CoreMsg(Play), isLevelFinished, _position)
import Game.Baseball.Model (State, Msg(..), _nbBases, _missingPeg)
import UI.Template (template, card)
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)

colors :: Array String
colors = ["blue", "red", "green", "magenta", "orange", "black", "cyan", "gray"]
dupColors :: Array String
dupColors = colors >>= \x -> [x, x]

translatePeg :: Int -> Int -> String
translatePeg position nbBases = translate (pc x) (pc y)  where
    mid = toNumber (position / 2)
    x = 0.42 + 0.35 * cos (mid * 2.0 * pi / toNumber nbBases) + 0.1 * toNumber (position `mod` 2)
    y = 0.46 + 0.35 * sin (mid * 2.0 * pi / toNumber nbBases)

transformBase :: Int -> Int -> String
transformBase i nbBases = translate (pc x) (pc y)  <> " rotate(45deg)" where
    x = 0.50 + 0.35 * cos (toNumber i * 2.0 * pi / toNumber nbBases)
    y = 0.50 + 0.35 * sin (toNumber i * 2.0 * pi / toNumber nbBases)

view :: State -> VDom Msg
view state = template _{config=config, board=board, rules=rules} state where
    nbBases = state^._nbBases
    levelFinished = isLevelFinished state

    config = card "Baseball multicolore" [
        iconSelectGroup state "Nombres de bases" [4, 5, 6, 7, 8] nbBases SetNbBases (const identity),
        icongroup "Options" $ [iundo state, iredo state, ireset state, irules state]
    ]

    board = div [class' "ui-board baseball-board" true] [
        svg [viewBox 0 0 100 100] $ concat [
            take nbBases colors # mapWithIndex \i color ->
                rect [
                    x_ "-10", y_ "-10", width "20", height "20",
                    key $ "b" <> show i,
                    class' "baseball-base" true,
                    stroke $ color,
                    style "transform" $ transformBase i nbBases
                ],
            map2 (state^._position) dupColors \peg pos color ->
                ifN (peg /= state^._missingPeg) \_ ->
                    g [
                        class' "baseball-player" true,
                        style "transform" $ translatePeg pos nbBases,
                        key $ "p" <> show peg
                    ] [ 
                        use "#meeple" [
                            width "7",
                            height "7",
                            onclick $ core (Play peg),
                            fill color,
                            style "animation"
                                if levelFinished then
                                    "baseballHola 4s linear " <> show (1000 + 2000 * peg / nbBases) <> "ms infinite"
                                else
                                    "none",
                            style "cursor" (if canPlay state peg then "pointer" else "not-allowed")
                        ]
                    ]
        ]
    ]

    rules = [text "blah blah blah blah"]
    