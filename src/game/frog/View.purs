module Game.Frog.View (view) where

import MamPrelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Lib.Util (map2, pairwise, rangeWithStep')
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (px, translate)
import Web.UIEvent.MouseEvent as ME
import Web.PointerEvent.PointerEvent as PE
import UI.Template (template, card, incDecGrid, turnMessage, winTitleFor2Players)
import UI.Icons (icongroup, iconSelectGroupM, icons2Players, ihelp, iundo, iredo, ireset, irules)
import Game.Core (_nbRows, _position, _help, _locked)
import Game.Frog.Model (Model, Msg(..), _moves, _marked, reachableArray)

type Cartesian = { x ∷ Number, y ∷ Number}
type Polar = { radius ∷ Number, theta ∷ Number }

rotate ∷ Number → String
rotate theta = "rotate(" <> show (theta * 180.0 / pi) <> "deg)"

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

drawSpiral ∷ ∀a. Html a
drawSpiral =
    H.g []
    [   H.path [P.d spiralPath, P.fill "none", P.stroke "black", P.strokeWidth 3.0]
    ,   H.line [P.x1 153.0, P.y1 9.0, P.x2 207.0, P.y2 20.0, P.stroke "black", P.strokeDasharray "5", P.strokeWidth 6.0]
    ,   H.line [P.x1 153.0, P.y1 7.0, P.x2 153.0, P.y2 39.0, P.stroke "black", P.strokeWidth 3.0]
    ,   H.line [P.x1 207.0, P.y1 18.0, P.x2 207.0, P.y2 50.0, P.stroke "black", P.strokeWidth 3.0]
    ]

lily ∷ ∀a. Int → Number → Number → Boolean → Boolean → Html a
lily i x y reachable hidden =
    H.use (pos <> 
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
        
view ∷ Model → Html Msg
view model = template {config, board, rules, winTitle} model where
    position = model ^. _position
    moves = model ^. _moves
    reachable = reachableArray model
    spoints = spiralPoints (model ^. _nbRows)
    pointsPolar = spiralPointsPolar $ model ^. _nbRows
    marked = model ^. _marked
    locked = model ^. _locked
    help = model ^. _help
    nbRows = model ^. _nbRows

    config =
        card "La grenouille"
        [   iconSelectGroupM model "Déplacements autorisés" [1, 2, 3, 4, 5] moves SelectMove (const identity)
        ,   icons2Players model
        ,   icongroup "Options" $ [ihelp, iundo, iredo, ireset, irules] <#> (_ $ model)
        ]

    drawFrog =
        H.maybe (pointsPolar !! position) \{radius, theta} →
            H.g
            [   H.class_ "frog-frog-container"
            ,   H.style "transform" $ rotate theta <> translate (px radius) "0"  <> rotate (-theta)
            ]
            [   H.use 
                [   P.href "#frog2"
                ,   P.x (-20.0)
                ,   P.y (-20.0)
                ,   P.width "40"
                ,   P.height "40"
                ,   H.class_ "frog-frog"
                ,   H.class' "goal" $ position == 0
                ]
            ]

    drawMarked = 
        H.g [] $
            map2 marked spoints \i mark {x, y} →
                H.when (mark && i ≠ position) \_ →
                    H.use 
                    [   P.href "#frog2"
                    ,   P.x $ x - 20.0
                    ,   P.y $ y - 20.0
                    ,   P.width "32"
                    ,   P.height "32"
                    ,   H.class_ "frog-frog marked"
                    ]
    
    drawLilypads =
        H.g [] $
            map2 spoints reachable \i {x, y} reach →
                H.g
                [   E.onClick \ev → if ME.shiftKey (PE.toMouseEvent ev) then Mark i else Play i
                ]
                [   lily i x y false false
                ,   lily i x y true (not reach || locked)
                ,   H.text_ (if help then show $ nbRows - i else "")
                    [   P.x x, P.y y, H.class_ "frog-index"
                    ]
                ]

    grid = 
        H.div [H.class_ "ui-board frog-board"]
        [   H.svg [P.viewBox (-190) (-200) 400 400]
            [   drawSpiral
            ,   drawLilypads
            ,   drawMarked
            ,   drawFrog
            ]
        ,   H.span [] [H.text (turnMessage model)]
        ]

    board = incDecGrid model [grid]

    rules =
        [   H.text "Le jeu de la grenouille est un jeu à deux joueurs."
        ,   H.br
        ,   H.text "A chaque tour, un joueur peut avancer la grenouille d'un nombre de cases parmi ceux indiqués dans \"Déplacements autorisés\"."
        ,   H.br
        ,   H.text "Le premier joueur à atteindre le nénuphar final a gagné."
        ,   H.br
        ,   H.text "Pour éviter une situation bloquante, un joueur peut se déplacer vers le nénuphar final en utilisant moins de déplacements que ce qui lui est autorisé."
        ,   H.br
        ,   H.text "Par exemple, si les mouvements autorisés sont {3, 4, 5}, le joueur a quand même le droit de se déplacer de 1 ou 2 cases si cela lui permet d'atteindre le nénuphar final."
    ]
    
    winTitle = winTitleFor2Players model