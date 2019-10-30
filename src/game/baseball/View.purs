module Game.Baseball.View where

import Prelude
import Data.Int (toNumber)
import Data.Array (catMaybes, take, mapWithIndex, concatMap)
import Math (cos, sin, pi)
import Data.Maybe (Maybe(..)) 
import Data.Lens (Lens', (^.))
import Pha (text)
import Pha.Class (VDom)
import Pha.Action ((🎲))
import Pha.Html (div', svg, g, rect, use, class', key, style,
            click, width, height, stroke, fill, viewBox, translate)
import Lib.Util (map2)
import Game.Core (canPlay, playA, isLevelFinished, _position)
import Game.Baseball.Model (BaseballState, setNbBases, _nbBases)
import UI.Template (template, card)
import UI.Icons (icongroup, iconSelectGroup, iundo, iredo, ireset, irules)

colors :: Array String
colors = ["blue", "red", "green", "magenta", "orange", "black", "cyan", "gray"]
dupColors :: Array String
dupColors = colors # concatMap \x -> [x, x]

translatePeg :: Int -> Int -> String
translatePeg position nbBases = translate x y  where
    mid = toNumber (position / 2)
    x = 42.0 + 35.0 * cos (mid * 2.0 * pi / toNumber nbBases) + 10.0 * toNumber (position `mod` 2)
    y = 46.0 + 35.0 * sin (mid * 2.0 * pi / toNumber nbBases)

transformBase :: Int -> Int -> String
transformBase i nbBases = translate x y  <> " rotate(45deg)" where
    x = 50.0 + 35.0 * cos (toNumber i * 2.0 * pi / toNumber nbBases)
    y = 50.0 + 35.0 * sin (toNumber i * 2.0 * pi / toNumber nbBases)

view :: forall a. Lens' a BaseballState -> BaseballState -> VDom a
view lens state = template lens {config, board, rules, winTitle} state where
    nbBases = state^._nbBases
    levelFinished = isLevelFinished state
    config =
        card "Baseball multicolore" [
            iconSelectGroup lens state "Nombres de bases" [4, 5, 6, 7, 8] (const identity) nbBases setNbBases,
            icongroup "Options" $ [ iundo, iredo, ireset, irules ] <#> \x -> x lens state
        ]

    board =
        div' [class' "ui-board baseball-board" true] [
            svg [width "100%", height "100%", viewBox "0 0 100 100"] $ 
                (take nbBases colors # mapWithIndex \i color ->
                    rect (-10.0) (-10.0) 20.0 20.0 [
                        key $ "b" <> show i,
                        class' "baseball-base" true,
                        stroke $ color,
                        style "transform" $ transformBase i nbBases
                    ]
                ) <> (catMaybes $ map2 (state^._position) dupColors \peg pos color ->
                    if peg == 0 then
                        Nothing
                    else
                        Just $ g [
                            class' "baseball-player" true,
                            style "transform" $ translatePeg pos nbBases,
                            key $ "p" <> show peg
                        ] [ 
                            use 0.0 0.0 7.0 7.0 "#meeple" [
                                click $ lens 🎲 playA peg,
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
    winTitle = "GAGNÉ"

    
    