module Game.Baseball.View where

import Prelude
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Array ((!!), (..), mapWithIndex, concat)
import Optic.Core (Lens', over)
import Math (cos, sin, pi)
import Pha (VDom, emptyNode)
import Pha.Html (div', svg, g, rect, use, class', key, style,
            click, width, href,
            height, x, y, stroke, fill, viewBox, transform)
import Lib.Game (State(St), canPlay, _play', isLevelFinished)
import Game.Baseball.Model (BaseballState, setNbBases)
import UI.Dialog (card)
import UI.Icon (icongroup, Icon(IconText))
import UI.Icons (iconbutton, iundo, iredo, ireset)


repeat :: forall a. Int -> (Int -> a) -> Array a
repeat n f = 0 .. (n - 1) # map f

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
view lens st@(St state) = 
    div' [] [
        div' [class' "main-container" true] [
            div' [] [board],
            --WinPanel()
            config
        ]
    
    --        ),
    --        state.dialog === 'rules' && O.HelpDialog(),
    --        state.dialog === 'confirm' && ConfirmDialog(),
    --        O.dialogs && state.dialog && O.dialogs[state.dialog] && O.dialogs[state.dialog]()
    ]

    where
    levelFinished = isLevelFinished st
    
    config =
        card "Baseball multicolore" [
            icongroup "nombres de bases" $ [4, 5, 6, 7, 8] # map(\i ->
                iconbutton st (_{
                    icon = IconText $ show i,
                    selected = state.nbBases == i
                }) [click $ over lens (setNbBases i)]
            ),
            icongroup "Options" [
                iundo lens st, iredo lens st, ireset lens st
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
                                click $ over lens (_play' player),
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