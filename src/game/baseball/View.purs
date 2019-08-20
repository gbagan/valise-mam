module Game.Baseball.View where

import Prelude
import Data.Int (toNumber)
import Data.Maybe (fromMaybe, Maybe(Nothing, Just))
import Data.Array ((!!), mapWithIndex, concat)
import Math (cos, sin, pi)
import Pha (VDom, emptyNode, text)
import Pha.Html (div', svg, g, rect, use, class', key, style,
            click, width, href,
            height, x, y, stroke, fill, viewBox, transform)
import Lib.Core (repeat)
import Lib.Random (Random)
import Lib.Game (State(St), canPlay, _play', isLevelFinished, setDialog, confirmNewGame,
                Dialog(NoDialog, Rules, ConfirmNewGame))
import Game.Baseball.Model (BaseballState, setNbBases)
import UI.Dialog (card, dialog)
import UI.Icon (icongroup, Icon(IconText))
import UI.Icons (iconbutton, iundo, iredo, ireset, irules, winPanel)

colors :: Array String
colors = ["blue", "red", "green", "magenta", "orange", "black", "cyan", "gray"]

translatePlayer :: Int -> Int -> String
translatePlayer position nbBases =
    "translate(" <> show x <> "%," <> show y <> "%)"
    where
        mid = toNumber (position / 2)
        x = 42.0 + 35.0 * cos (mid * 2.0 * pi / toNumber nbBases) + toNumber (10 * (position `mod` 2))
        y = 46.0 + 35.0 * sin (mid * 2.0 * pi / toNumber nbBases)

transformBase :: Int -> Int -> String
transformBase i nbBases =
    "translate(" <> show x <> "," <> show y <> ") rotate(45)"
    where
        x = 500.0 + 350.0 * cos (toNumber i * 2.0 * pi / toNumber nbBases)
        y = 500.0 + 350.0 * sin (toNumber i * 2.0 * pi / toNumber nbBases)

view :: forall a. ((BaseballState -> BaseballState) -> a -> a) ->
    ((BaseballState -> Random BaseballState) -> a -> a)
    -> BaseballState -> VDom a
view action rndaction st@(St state) = 
    div' [] [
        div' [class' "main-container" true] [
            div' [] [board, winPanel st],
            config
        ],
    
        dialog' state.dialog
    ]

    where
    levelFinished = isLevelFinished st
    
    config =
        card "Baseball multicolore" [
            icongroup "nombres de bases" $ [4, 5, 6, 7, 8] # map(\i ->
                iconbutton st (_{
                    icon = IconText $ show i,
                    selected = state.nbBases == i
                }) [click $ rndaction (setNbBases i)]
            ),
            icongroup "Options" [
                iundo action st, iredo action st, ireset action st, irules action st
            ]
        ]

    board =
        div' [class' "ui-board baseball-board" true] [
            svg [width "100%", height "100%", viewBox "0 0 1000 1000"] $ concat [
                repeat state.nbBases (\i ->
                    rect [
                        key ("b" <> show i),
                        class' "baseball-base" true,
                        stroke (fromMaybe "black" $ colors !! i),
                        width "200",
                        height "200",
                        x "-100",
                        y "-100",
                        transform $ transformBase i state.nbBases
                    ] []
                ),
                state.position # mapWithIndex (\player position ->
                    if player == 0 then
                        emptyNode
                    else
                        g [
                            class' "baseball-player" true,
                            style "transform" $ translatePlayer position state.nbBases,
                            key $ "p" <> show player
                        ] [ 
                            use [
                                href "#meeple",
                                click $ action (_play' player),
                                width "70",
                                height "70",
                                fill $ fromMaybe "black" (colors !! (player / 2)),
                                style "animation"
                                    if levelFinished then
                                        "baseballHola 4s linear " <> show (1000 + 2000 * player / state.nbBases)
                                        <> "ms infinite"
                                    else
                                        "none",
                                style "cursor" $ if canPlay st player then "pointer" else "not-allowed"
                            ] []
                        ]
                )
            ]
        ]

    dialog' Rules =
        dialog {title: "Règles", onOk: Just (action $ setDialog NoDialog), onCancel: Nothing} [
            text "blah blah blah blah"
        ]

    dialog' (ConfirmNewGame s) =
        dialog {title: "Règles", onCancel: Just (action $ setDialog NoDialog), onOk: Just (action $ confirmNewGame s)} [
            text "blah blah blah blah"
        ]
    dialog' _ = emptyNode
    
    