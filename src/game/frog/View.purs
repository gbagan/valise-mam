module Game.Frog.View (view) where

import MyPrelude
import Lib.Util (map2, tabulate, pairwise, floatRange)
import Pha (VDom, text, (<&&>), (<??>), key, class_, class', style)
import Pha.Elements (div, span, br)
import Pha.Events (on)
import Pha.Events.Decoder (shiftKey)
import Pha.Svg (svg, g, use, line, path, text', x_, y_, width, height, viewBox, stroke, fill, strokeDasharray, strokeWidth)
import Pha.Util (px, translate)
import UI.Template (template, card, incDecGrid, turnMessage, winTitleFor2Players)
import UI.Icons (icongroup, iconSelectGroupM, icons2Players, ihelp, iundo, iredo, ireset, irules)
import Game.Core (_nbRows, _position, _help, _locked)
import Game.Frog.Model (State, Msg(..), _moves, _marked, reachableArray)

type Cartesian = { x ∷ Number, y ∷ Number}
type Polar = { radius ∷ Number, theta ∷ Number }

lineIntersection ∷ Number → Number → Number → Number → { x ∷ Number, y ∷ Number }
lineIntersection  m1 b1 m2 b2 = { x, y: m1 * x + b1 } where x = (b2 - b1) / (m1 - m2)

polarToCartesian ∷ Polar → Cartesian
polarToCartesian {radius, theta} = { x: radius * cos theta, y: radius * sin theta }

spiral ∷ Cartesian → Number → Number → Number → Number → Number → String
spiral center startRadius radiusStep startTheta endTheta thetaStep =
    floatRange startTheta endTheta thetaStep <#> (\theta →
        let b = radiusStep / (2.0 * pi)
            r = startRadius + b * theta
            point = { x: center.x + r * cos theta, y: center.y + r * sin theta }
            slope = (b * sin theta + r * cos theta) / (b * cos theta - r * sin theta)
            intercept = -(slope * r * cos theta - r * sin theta)
        in { point, slope, intercept }
    )
    # pairwise
    # mapWithIndex (\i (a ∧ b) →
        let { x, y } = lineIntersection a.slope a.intercept b.slope b.intercept
            p = ["Q", show $ x + center.x, show $ y + center.y, show $ b.point.x, show b.point.y]
        in
            if i == 0 then ["M", show a.point.x, show a.point.y] <> p else p
    )
    # concat
    # joinWith " "

spiralPointsPolar ∷ Int → Array Polar
spiralPointsPolar n = reverse $ tabulate (n + 1) \i →
    let theta = sqrt(if i == n then 21.0 else toNumber i * 20.0 / toNumber n) * 1.36 * pi
        radius = 61.0 * theta / (2.0 * pi)
    in { theta, radius }


spiralPoints ∷ Int → Array Cartesian
spiralPoints n = spiralPointsPolar n <#> polarToCartesian

spiralPath ∷ String
spiralPath = spiral { x: 0.0, y: 0.0 } 0.0 61.0 0.0 (37.0 / 6.0 * pi) (pi / 6.0)

lily ∷ ∀a. Int → Number → Number → Boolean → Boolean → VDom a
lily i x y reachable hidden =
    use "#lily" (pos <> [
        class_ "frog-lily",
        class' "reachable" reachable,
        class' "hidden" hidden
    ]) where
    pos = if i == 0 then
            [x_ $ show (x - 30.0), y_ $ show (y - 45.0), width "80", height "80"] 
        else
            [x_ $ show (x - 24.0), y_ $ show (y - 24.0), width "48", height "48"]
        

view ∷ State → VDom Msg
view state = template {config, board, rules, winTitle} state where
    position = state^._position
    reachable = reachableArray state
    spoints = spiralPoints (state^._nbRows)
    pointsPolar = spiralPointsPolar $ state^._nbRows
    config = card "La grenouille" [
        iconSelectGroupM state "Déplacements autorisés" [1, 2, 3, 4, 5] (state^._moves) SelectMove (const identity),
        icons2Players state,
        icongroup "Options" $ [ihelp, iundo, iredo, ireset, irules] <#> \x → x state
    ]
    grid = 
        div [class_ "ui-board frog-board"] [
            svg [viewBox (-190) (-200) 400 400] $ concat [
                [
                    path spiralPath [fill "none", stroke "black", strokeWidth "3"],
                    line [x_ "153", y_ "9", width "207", height "20", stroke "black", strokeDasharray "5", strokeWidth "6"],
                    line [x_ "153", y_ "7", width "153", height "39", stroke "black", strokeWidth "3"],
                    line [x_ "207", y_"18", width "207", height "50", stroke "black", strokeWidth "3"]
                ],
                map2 spoints reachable \i {x, y} reach →
                    g [
                        key $ "lily" <> show i,
                        on "click" $ shiftKey >>> map (if _ then Mark i else Play i)
                    ] [
                        lily i x y false false,
                        lily i x y true (not reach || state^._locked),
                        text' (if state^._help then show $ (state^._nbRows) - i else "") [
                            x_ $ show x, y_ $ show y, class_ "frog-index"
                        ]
                    ],
                map2 (state^._marked) spoints \i mark {x, y} →
                    mark && i /= position <&&> \_ →
                        use "#frog2" [
                            x_ $ show (x - 20.0),
                            y_ $ show (y - 20.0),
                            width "32",
                            height "32",
                            key $ "reach" <> show i,
                            class_ "frog-frog marked"
                        ],
                [pointsPolar !! position <??> \{radius, theta} →
                    g [
                    key "frog",
                    class_ "frog-frog-container",
                    style "transform" $ translate (px radius) "0" <> " rotate(" <> show (theta * 180.0 / pi) <> "deg)",
                    style "transform-origin" $ px (-radius) <> " 0"
                ] [
                    g [
                        class_ "frog-frog-container",
                        style "transform" $ "rotate(" <> show (-theta * 180.0 / pi) <> "deg)"
                    ] [
                        use "#frog2" [
                            x_ "-20", y_ "-20", width "40", height "40",
                            class_ "frog-frog",
                            class' "goal" $ position == 0
                        ]
                    ]
                ]]
            ],
            span [] [text (turnMessage state)]
        ]

    board = incDecGrid state [grid]

    rules = [
        text "Jeu de la grenouille", br,
        text "Règles pas encore définies"
    ]
    
    winTitle = winTitleFor2Players state