module Game.Baseball.View where

import MyPrelude
import Lib.Util (map2)
import Pha (VDom, text, ifN)
import Pha.Action ((🔍))
import Pha.Html (div', svg, g, rect, use, class', key, style, click, pc, stroke, fill, viewBox, translate)
import Game.Effs (EFFS)
import Game.Core (canPlay, playA, isLevelFinished, _position)
import Game.Baseball.Model (State, setNbBasesA, _nbBases, _missingPeg)
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

view :: ∀a. Lens' a State -> State -> VDom a EFFS
view lens state = template lens (_{config=config, board=board, rules=rules}) state where
    nbBases = state^._nbBases
    levelFinished = isLevelFinished state

    config = card "Baseball multicolore" [
        iconSelectGroup lens state "Nombres de bases" [4, 5, 6, 7, 8] nbBases setNbBasesA (const identity),
        icongroup "Options" $ [ iundo, iredo, ireset, irules ] <#> \x -> x lens state
    ]

    board = div' [class' "ui-board baseball-board" true] [
        svg [viewBox 0 0 100 100] $ 
            (take nbBases colors # mapWithIndex \i color ->
                rect (-10.0) (-10.0) 20.0 20.0 [
                    key $ "b" <> show i,
                    class' "baseball-base" true,
                    stroke $ color,
                    style "transform" $ transformBase i nbBases
                ]
            ) <> (map2 (state^._position) dupColors \peg pos color ->
                ifN (peg /= state^._missingPeg) \_ ->
                    g [
                        class' "baseball-player" true,
                        style "transform" $ translatePeg pos nbBases,
                        key $ "p" <> show peg
                    ] [ 
                        use 0.0 0.0 7.0 7.0 "#meeple" [
                            click $ lens 🔍 playA peg,
                            fill $ color,
                            style "animation"
                                if levelFinished then
                                    "baseballHola 4s linear " <> show (1000 + 2000 * peg / nbBases)
                                    <> "ms infinite"
                                else
                                    "none",
                            style "cursor" $ if canPlay state peg then "pointer" else "not-allowed"
                        ]
                    ]
            )
    ]

    rules = [text "blah blah blah blah"]
    