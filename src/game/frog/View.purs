module Game.Frog.View (view) where

import MyPrelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Lib.Util (map2, repeat, pairwise, rangeWithStep')
import Pha as H
import Pha.Elements as HH
import Pha.Attributes as P
import Pha.Events as E
import Pha.Util (px, translate)
import Web.UIEvent.MouseEvent as ME
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
    rangeWithStep' startTheta endTheta thetaStep <#> (\theta →
        let b = radiusStep / (2.0 * pi)
            r = startRadius + b * theta
            point = { x: center.x + r * cos theta, y: center.y + r * sin theta }
            slope = (b * sin theta + r * cos theta) / (b * cos theta - r * sin theta)
            intercept = -(slope * r * cos theta - r * sin theta)
        in { point, slope, intercept }
    )
    # pairwise
    # foldMapWithIndex (\i (a ∧ b) →
        let { x, y } = lineIntersection a.slope a.intercept b.slope b.intercept
            p = ["Q", show $ x + center.x, show $ y + center.y, show $ b.point.x, show b.point.y]
        in
            if i == 0 then ["M", show a.point.x, show a.point.y] <> p else p
    )
    # joinWith " "

spiralPointsPolar ∷ Int → Array Polar
spiralPointsPolar n = reverse $ repeat (n + 1) \i →
    let theta = sqrt(if i == n then 21.0 else toNumber i * 20.0 / toNumber n) * 1.36 * pi
        radius = 61.0 * theta / (2.0 * pi)
    in { theta, radius }


spiralPoints ∷ Int → Array Cartesian
spiralPoints n = spiralPointsPolar n <#> polarToCartesian

spiralPath ∷ String
spiralPath = spiral { x: 0.0, y: 0.0 } 0.0 61.0 0.0 (37.0 / 6.0 * pi) (pi / 6.0)

drawSpiral ∷ ∀a. Array (H.VDom a)
drawSpiral =
    [   HH.path [P.d spiralPath, P.fill "none", P.stroke "black", P.strokeWidth "3"]
    ,   HH.line [P.x1 153.0, P.y1 9.0, P.x2 207.0, P.y2 20.0, P.stroke "black", P.strokeDasharray "5", P.strokeWidth "6"]
    ,   HH.line [P.x1 153.0, P.y1 7.0, P.x2 153.0, P.y2 39.0, P.stroke "black", P.strokeWidth "3"]
    ,   HH.line [P.x1 207.0, P.y1 18.0, P.x2 207.0, P.y2 50.0, P.stroke "black", P.strokeWidth "3"]
    ]

lily ∷ ∀a. Int → Number → Number → Boolean → Boolean → H.VDom a
lily i x y reachable hidden =
    HH.use (pos <> 
        [   P.href "#lily"
        ,   H.class_ "frog-lily"
        ,   H.class' "reachable" reachable
        ,   H.class' "hidden" hidden
    ]) where
    pos = if i == 0 then
            [   P.x $ x - 30.0
            ,   P.y $ y - 45.0
            ,   P.width "80"
            ,   P.height "80"
            ] 
        else
            [   P.x $ x - 24.0
            ,   P.y $ y - 24.0
            ,   P.width "48"
            ,   P.height "48"
            ]
        
view ∷ State → H.VDom Msg
view state = template {config, board, rules, winTitle} state where
    position = state ^. _position
    moves = state ^. _moves
    reachable = reachableArray state
    spoints = spiralPoints (state ^. _nbRows)
    pointsPolar = spiralPointsPolar $ state ^. _nbRows
    marked = state ^. _marked
    locked = state ^. _locked
    help = state ^. _help
    nbRows = state ^. _nbRows

    config =
        card "La grenouille"
        [   iconSelectGroupM state "Déplacements autorisés" [1, 2, 3, 4, 5] moves SelectMove (const identity)
        ,   icons2Players state
        ,   icongroup "Options" $ [ihelp, iundo, iredo, ireset, irules] <#> (_ $ state)
        ]

    drawFrog =
        H.maybe (pointsPolar !! position) \{radius, theta} →
            HH.g
            [   H.key "frog"
            ,   H.class_ "frog-frog-container"
            ,   H.style "transform" $ translate (px radius) "0" <> " rotate(" <> show (theta * 180.0 / pi) <> "deg)"
            ,   H.style "transform-origin" $ px (-radius) <> " 0"
            ]
            [   HH.g
                [   H.class_ "frog-frog-container"
                ,   H.style "transform" $ "rotate(" <> show (-theta * 180.0 / pi) <> "deg)"
                ]
                [   HH.use 
                    [   P.href "#frog2"
                    ,   P.x (-20.0)
                    ,   P.y (-20.0)
                    ,   P.width "40"
                    ,   P.height "40"
                    ,   H.class_ "frog-frog"
                    ,   H.class' "goal" $ position == 0
                    ]
                ]
            ]

    drawMarked =
        map2 marked spoints \i mark {x, y} →
            H.when (mark && i ≠ position) \_ →
                HH.use 
                [   P.href "#frog2"
                ,   P.x $ x - 20.0
                ,   P.y $ y - 20.0
                ,   P.width "32"
                ,   P.height "32"
                ,   H.key $ "reach" <> show i
                ,   H.class_ "frog-frog marked"
                ]
    
    drawLilypads =
        map2 spoints reachable \i {x, y} reach →
            HH.g
            [   H.key $ "lily" <> show i
            ,   E.onclick_ \ev → pure $ Just $ if ME.shiftKey ev then Mark i else Play i
            ]
            [   lily i x y false false
            ,   lily i x y true (not reach || locked)
            ,   HH.text (if help then show $ nbRows - i else "")
                [   P.x x, P.y y, H.class_ "frog-index"
                ]
            ]

    grid = 
        HH.div [H.class_ "ui-board frog-board"]
        [   HH.svg [P.viewBox (-190) (-200) 400 400] $ concat
            [   drawSpiral
            ,   drawLilypads
            ,   drawMarked
            ,   [drawFrog]
            ]
        ,   HH.span [] [H.text (turnMessage state)]
        ]

    board = incDecGrid state [grid]

    rules =
        [   H.text "Le jeu de la grenouille est un jeu à deux joueurs."
        ,   HH.br
        ,   H.text "A chaque tour, un joueur peut avancer la grenouille d'un nombre de cases parmi ceux indiqués dans \"Déplacements autorisés\"."
        ,   HH.br
        ,   H.text "Le premier joueur à atteindre le nénuphar final a gagné."
        ,   HH.br
        ,   H.text "Pour éviter une situation bloquante, un joueur peut se déplacer vers le nénuphar final en utilisant moins de déplacements que ce qui lui est autorisé."
        ,   HH.br
        ,   H.text "Par exemple, si les mouvements autorisés sont {3, 4, 5}, le joueur a quand même le droit de se déplacer de 1 ou 2 cases si cela lui permet d'atteindre le nénuphar final."
    ]
    
    winTitle = winTitleFor2Players state