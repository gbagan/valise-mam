module Game.Frog.View (view) where

import MamPrelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Lib.Helpers (map2, rangeWithStep', windows2)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (px, translate)
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import Web.UIEvent.MouseEvent as ME
import UI.Template (template, card, incDecGrid, turnMessage, winTitleFor2Players)
import UI.Icons (icongroup, iconSelectGroupM, icons2Players, ihelp, iundo, iredo, ireset, irules)
import Game.Core (_nbRows, _position, _help, _locked)
import Game.Frog.Model (Model, Msg(..), _moves, _marked, reachableArray)

type Cartesian = { x ∷ Number, y ∷ Number }
type Polar = { radius ∷ Number, theta ∷ Number }

rotate ∷ Number → String
rotate theta = "rotate(" <> show (theta * 180.0 / pi) <> "deg)"

lineIntersection ∷ Number → Number → Number → Number → { x ∷ Number, y ∷ Number }
lineIntersection m1 b1 m2 b2 = { x, y: m1 * x + b1 }
  where
  x = (b2 - b1) / (m1 - m2)

polarToCartesian ∷ Polar → Cartesian
polarToCartesian { radius, theta } = { x: radius * cos theta, y: radius * sin theta }

spiral ∷ Cartesian → Number → Number → Number → Number → Number → String
spiral center startRadius radiusStep startTheta endTheta thetaStep =
  rangeWithStep' startTheta endTheta thetaStep
    # map
      ( \theta →
          let
            b = radiusStep / (2.0 * pi)
            r = startRadius + b * theta
            point = { x: center.x + r * cos theta, y: center.y + r * sin theta }
            slope = (b * sin theta + r * cos theta) / (b * cos theta - r * sin theta)
            intercept = -(slope * r * cos theta - r * sin theta)
          in
            { point, slope, intercept }
      )
    # windows2
    # foldMapWithIndex
        ( \i (a ∧ b) →
            let
              { x, y } = lineIntersection a.slope a.intercept b.slope b.intercept
              p = [ "Q", show $ x + center.x, show $ y + center.y, show $ b.point.x, show b.point.y ]
            in
              if i == 0 then [ "M", show a.point.x, show a.point.y ] <> p else p
        )
    # joinWith " "

spiralPointsPolar ∷ Int → Array Polar
spiralPointsPolar n = reverse $ repeat (n + 1) \i →
  let
    theta = sqrt (if i == n then 21.0 else toNumber i * 20.0 / toNumber n) * 1.36 * pi
    radius = 61.0 * theta / (2.0 * pi)
  in
    { theta, radius }

spiralPoints ∷ Int → Array Cartesian
spiralPoints n = polarToCartesian <$> spiralPointsPolar n

spiralPath ∷ String
spiralPath = spiral { x: 0.0, y: 0.0 } 0.0 61.0 0.0 (37.0 / 6.0 * pi) (pi / 6.0)

drawSpiral ∷ ∀ a. Html a
drawSpiral =
  S.g []
    [ S.path [ SA.d spiralPath, SA.fill "none", SA.stroke "black", SA.strokeWidth 3.0 ]
    , S.line [ SA.x1 153, SA.y1 9, SA.x2 207, SA.y2 20, SA.stroke "black", SA.strokeDasharray "5", SA.strokeWidth 6.0 ]
    , S.line [ SA.x1 153, SA.y1 7, SA.x2 153, SA.y2 39, SA.stroke "black", SA.strokeWidth 3.0 ]
    , S.line [ SA.x1 207, SA.y1 18, SA.x2 207, SA.y2 50.0, SA.stroke "black", SA.strokeWidth 3.0 ]
    ]

lily ∷ ∀ a. Int → Number → Number → Boolean → Boolean → Html a
lily i x y reachable hidden =
  S.use
    ( pos <>
        [ P.href "#lily"
        , H.class_ "frog-lily"
        , H.class' "reachable" reachable
        , H.class' "hidden" hidden
        ]
    )
  where
  pos =
    if i == 0 then
      [ SA.x $ x - 30.0
      , SA.y $ y - 45.0
      , SA.width 80
      , SA.height 80
      ]
    else
      [ SA.x $ x - 24.0
      , SA.y $ y - 24.0
      , SA.width 48
      , SA.height 48
      ]

view ∷ Model → Html Msg
view model = template { config, board, rules, winTitle } model
  where
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
      [ iconSelectGroupM model "Déplacements autorisés" [ 1, 2, 3, 4, 5 ] moves SelectMove (const identity)
      , icons2Players model
      , icongroup "Options" $ [ ihelp, iundo, iredo, ireset, irules ] # map (_ $ model)
      ]

  drawFrog =
    H.maybe (pointsPolar !! position) \{ radius, theta } →
      S.g
        [ H.class_ "frog-frog-container"
        , H.style "transform" $ rotate theta <> translate (px radius) "0" <> rotate (-theta)
        ]
        [ S.use
            [ P.href "#frog2"
            , SA.x (-20)
            , SA.y (-20)
            , SA.width 40
            , SA.height 40
            , H.class_ "frog-frog"
            , H.class' "goal" $ position == 0
            ]
        ]

  drawMarked =
    S.g [] $
      map2 marked spoints \i mark { x, y } →
        H.when (mark && i ≠ position) \_ →
          S.use
            [ P.href "#frog2"
            , SA.x $ x - 20.0
            , SA.y $ y - 20.0
            , SA.width 32
            , SA.height 32
            , H.class_ "frog-frog marked"
            ]

  drawLilypads =
    S.g [] $
      map2 spoints reachable \i { x, y } reach →
        S.g
          [ E.onClick \ev → if ME.shiftKey ev then Mark i else Play i
          ]
          [ lily i x y false false
          , lily i x y true (not reach || locked)
          , S.text
              [ SA.x x, SA.y y, H.class_ "frog-index" ]
              [ H.text if help then show $ nbRows - i else "" ]
          ]

  grid =
    H.div [ H.class_ "ui-board frog-board" ]
      [ S.svg [ SA.viewBox (-190.0) (-200.0) 400.0 400.0 ]
          [ drawSpiral
          , drawLilypads
          , drawMarked
          , drawFrog
          ]
      , H.span [] [ H.text (turnMessage model) ]
      ]

  board = incDecGrid model [ grid ]

  rules =
    [ H.text "Le jeu de la grenouille est un jeu à deux joueurs."
    , H.br
    , H.text "A chaque tour, un joueur peut avancer la grenouille d'un nombre de cases parmi ceux indiqués dans \"Déplacements autorisés\"."
    , H.br
    , H.text "Le premier joueur à atteindre le nénuphar final a gagné."
    , H.br
    , H.text "Pour éviter une situation bloquante, un joueur peut se déplacer vers le nénuphar final en utilisant moins de déplacements que ce qui lui est autorisé."
    , H.br
    , H.text "Par exemple, si les mouvements autorisés sont {3, 4, 5}, le joueur a quand même le droit de se déplacer de 1 ou 2 cases si cela lui permet d'atteindre le nénuphar final."
    ]

  winTitle = winTitleFor2Players model