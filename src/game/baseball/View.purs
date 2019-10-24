module Game.Baseball.View where

import Prelude
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Array ((!!), mapWithIndex, concat)
import Math (cos, sin, pi)
import Optic.Core (Lens', (^.))
import Pha (VDom, emptyNode, text, lensAction)
import Pha.Html (div', svg, g, rect, use, class', key, style,
            click, width, href,
            height, x, y, stroke, fill, viewBox, transform)
import Lib.Core (repeat)
import Lib.Game ((🎲), canPlay, _play', isLevelFinished, _position)
import Game.Baseball.Model (BaseballState, setNbBases, _nbBases)
import UI.Template (template)
import UI.Dialog (card)
import UI.Icon (icongroup, Icon(IconText))
import UI.Icons (iconbutton, iundo, iredo, ireset, irules)

colors :: Array String
colors = ["blue", "red", "green", "magenta", "orange", "black", "cyan", "gray"]

translatePeg :: Int -> Int -> String
translatePeg position nbBases =
    "translate(" <> show x <> "%," <> show y <> "%)"
    where
        mid = toNumber (position / 2)
        x = 42.0 + 35.0 * cos (mid * 2.0 * pi / toNumber nbBases) + 10.0 * toNumber (position `mod` 2)
        y = 46.0 + 35.0 * sin (mid * 2.0 * pi / toNumber nbBases)

transformBase :: Int -> Int -> String
transformBase i nbBases =
    "translate(" <> show x <> "," <> show y <> ") rotate(45)"
    where
        x = 500.0 + 350.0 * cos (toNumber i * 2.0 * pi / toNumber nbBases)
        y = 500.0 + 350.0 * sin (toNumber i * 2.0 * pi / toNumber nbBases)

view :: forall a. Lens' a BaseballState -> BaseballState -> VDom a
view lens state = template lens elements state where
    nbBases = state^._nbBases
    levelFinished = isLevelFinished state
    elements = {
        config:
            card "Baseball multicolore" [
                icongroup "nombres de bases" $ [4, 5, 6, 7, 8] # map \i ->
                    iconbutton state (_{
                        icon = IconText $ show i,
                        selected = nbBases == i
                    }) [click $ lens 🎲 setNbBases i]
                ,
                icongroup "Options" $ [ iundo, iredo, ireset, irules ] <#> \x -> x lens state
            ],

        board:
            div' [class' "ui-board baseball-board" true] [
                svg [width "100%", height "100%", viewBox "0 0 1000 1000"] $ concat [
                    repeat nbBases \i ->
                        rect [
                            key ("b" <> show i),
                            class' "baseball-base" true,
                            stroke $ fromMaybe "black" $ colors !! i,
                            width "200",
                            height "200",
                            x "-100",
                            y "-100",
                            transform $ transformBase i nbBases
                        ] [],
                        state^._position # mapWithIndex \peg pos ->
                        if peg == 0 then
                            emptyNode
                        else
                            g [
                                class' "baseball-player" true,
                                style "transform" $ translatePeg pos nbBases,
                                key $ "p" <> show peg
                            ] [ 
                                use [
                                    href "#meeple",
                                    click $ lensAction lens $ _play' peg,
                                    width "70",
                                    height "70",
                                    fill $ fromMaybe "black" $ colors !! (peg / 2),
                                    style "animation"
                                        if levelFinished then
                                            "baseballHola 4s linear " <> show (1000 + 2000 * peg / nbBases)
                                            <> "ms infinite"
                                        else
                                            "none",
                                    style "cursor" $ if canPlay state peg then "pointer" else "not-allowed"
                                ] []
                            ]
                ]
            ],
        rules: [text "blah blah blah blah"]
    }

    
    