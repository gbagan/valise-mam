module Game.Baseball.View where

import Prelude
import Data.Int (toNumber)
import Data.Maybe (fromMaybe, Maybe(Nothing, Just))
import Data.Array ((!!), mapWithIndex, concat)
import Math (cos, sin, pi)
import Optic.Core (Lens', (^.), (.~))
import Pha (VDom, emptyNode, text, lensAction, action, rndAction)
import Pha.Html (div', svg, g, rect, use, class', key, style,
            click, width, href,
            height, x, y, stroke, fill, viewBox, transform)
import Lib.Core (repeat)
import Lib.Random (Random)
import Lib.Game (canPlay, _play', isLevelFinished, confirmNewGame,
                Dialog(NoDialog, Rules, ConfirmNewGame), _position, _dialog)
import Game.Baseball.Model (BaseballState, setNbBases, _nbBases)
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

view :: forall a. Lens' a BaseballState -> BaseballState -> VDom a
view lens state = 
    div' [] [
        div' [class' "main-container" true] [
            div' [] [board, winPanel state],
            config
        ],
    
        dialog' (state^._dialog)
    ]

    where
    position = state^._position
    nbBases = state^._nbBases
    levelFinished = isLevelFinished state
    laction = lensAction lens <<< action
    
    config =
        card "Baseball multicolore" [
            icongroup "nombres de bases" $ [4, 5, 6, 7, 8] # map(\i ->
                iconbutton state (_{
                    icon = IconText $ show i,
                    selected = nbBases == i
                }) [click $ lensAction lens $ rndAction (setNbBases i)]
            ),
            icongroup "Options" [
                iundo lens state, iredo lens state, ireset lens state, irules lens state
            ]
        ]

    board =
        div' [class' "ui-board baseball-board" true] [
            svg [width "100%", height "100%", viewBox "0 0 1000 1000"] $ concat [
                repeat nbBases (\i ->
                    rect [
                        key ("b" <> show i),
                        class' "baseball-base" true,
                        stroke (fromMaybe "black" $ colors !! i),
                        width "200",
                        height "200",
                        x "-100",
                        y "-100",
                        transform $ transformBase i nbBases
                    ] []
                ),
                position # mapWithIndex (\player pos ->
                    if player == 0 then
                        emptyNode
                    else
                        g [
                            class' "baseball-player" true,
                            style "transform" $ translatePlayer pos nbBases,
                            key $ "p" <> show player
                        ] [ 
                            use [
                                href "#meeple",
                                click $ laction (_play' player),
                                width "70",
                                height "70",
                                fill $ fromMaybe "black" (colors !! (player / 2)),
                                style "animation"
                                    if levelFinished then
                                        "baseballHola 4s linear " <> show (1000 + 2000 * player / nbBases)
                                        <> "ms infinite"
                                    else
                                        "none",
                                style "cursor" $ if canPlay state player then "pointer" else "not-allowed"
                            ] []
                        ]
                )
            ]
        ]

    dialog' Rules =
        dialog {title: "Règles", onOk: Just (laction $ _dialog .~ NoDialog), onCancel: Nothing} [
            text "blah blah blah blah"
        ]

    dialog' (ConfirmNewGame s) =
        dialog {title: "Règles", onCancel: Just (laction $ _dialog .~ NoDialog), onOk: Just (laction $ confirmNewGame s)} [
            text "blah blah blah blah"
        ]
    dialog' _ = emptyNode
    
    