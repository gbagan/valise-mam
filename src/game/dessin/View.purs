module Game.Dessin.View (view) where

import MamPrelude
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import Game.Core (canPlay, isLevelFinished, _position, _pointer)
import Lib.Graph (Position)
import Lib.Graph as Graph
import Game.Dessin.Model
  ( Model
  , Msg(..)
  , Move(..)
  , GraphIndex(..)
  , graphs
  , nbGraphs
  , edgesOf
  , nbRaises
  , _graph
  , _graphIndex
  , _graphEditor
  )
import UI.Template (template, card, trackPointer, bestScoreDialog)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup, iconBestScore, iundo, iredo, ireset, irules)
import UI.GraphEditor as GEditor

currentLine ∷ ∀ a. Position → Position → Html a
currentLine p1 p2 =
  S.line
    [ SA.x1 $ 100.0 * p1.x
    , SA.y1 $ 100.0 * p1.y
    , SA.x2 $ 100.0 * p2.x
    , SA.y2 $ 100.0 * p2.y
    , H.class_ "dessin-line-to-pointer"
    ]

view ∷ Model → Html Msg
view model = template { config, board, rules, winTitle, scoreDialog, customDialog } model
  where
  position = model ^. _position
  graph = model ^. _graph
  graphIndex = model ^. _graphIndex
  raises = nbRaises model
  s = if raises > 1 then "s" else ""
  levelFinished = isLevelFinished model

  config =
    card "Dessin"
      [ iconSelectGroup model "Niveau" ((GraphIndex <$> 0 .. (nbGraphs - 1)) <> [ CustomGraph ]) graphIndex SetGraphIndex
          case _ of
            GraphIndex i → _ { icon = IconText (show (i + 1)), tooltip = graphs !! i # map _.title }
            _ → _ { icon = IconSymbol "#customize", tooltip = Just "Crée ta propre pièce" }
      , icongroup "Options" [ iundo model, iredo model, ireset model, irules model ]
      , iconBestScore model
      ]

  board =
    H.div
      ( trackPointer <>
          [ H.class_ "ui-board dessin-board"
          , E.onContextMenuPrevent \_ → Play Raise
          ]
      )
      [ S.svg [ H.class_ "dessin-svg", SA.viewBox 0.0 0.0 100.0 100.0 ] $ concat
          [ graph.edges # map \edge →
              H.maybe (Graph.getCoordsOfEdge graph edge) \{ px1, px2, py1, py2 } →
                S.line
                  [ SA.x1 $ 100.0 * px1
                  , SA.y1 $ 100.0 * py1
                  , SA.x2 $ 100.0 * px2
                  , SA.y2 $ 100.0 * py2
                  , H.class_ "dessin-line1"
                  ]
          , edgesOf (model ^. _position) # map \edge →
              H.maybe (Graph.getCoordsOfEdge graph edge) \{ px1, px2, py1, py2 } →
                S.line
                  [ SA.x1 $ 100.0 * px1
                  , SA.y1 $ 100.0 * py1
                  , SA.x2 $ 100.0 * px2
                  , SA.y2 $ 100.0 * py2
                  , H.class_ "dessin-line2"
                  ]
          , if not levelFinished then
              graph.vertices # mapWithIndex \i { x, y } →
                S.circle
                  [ SA.cx $ 100.0 * x
                  , SA.cy $ 100.0 * y
                  , SA.r 3.0
                  , SA.stroke $ if Just (MoveTo i) == last position then "red" else "blue"
                  , SA.fill "blue"
                  , E.onClick \_ → Play (MoveTo i)
                  ]
            else
              []
          , [ H.when (not levelFinished) \_ →
                H.fromMaybe case last position of
                  Just (MoveTo x) → currentLine <$> (model ^. _pointer) <*> (Graph.getCoords graph x)
                  _ → Nothing
            ]
          ]
      , H.span [ H.class_ "dessin-title" ]
          [ H.text $ graph.title
          ]
      , H.span [ H.class_ "dessin-raise-info" ]
          [ H.text $ show raises <> " levé" <> s <> " de crayon"
          ]
      , H.button
          [ H.class_ "ui-button ui-button-primary dessin-raise"
          , P.disabled $ not (canPlay model Raise) || levelFinished
          , E.onClick \_ → Play Raise
          ]
          [ H.text "Lever le crayon" ]
      ]

  scoreDialog _ = bestScoreDialog model \bestPos →
    [ H.div [ H.class_ "ui-board dessin-bestscore" ]
        [ S.svg [ H.class_ "dessin-svg", SA.viewBox 0.0 0.0 100.0 100.0 ] $ concat
            [ graph.edges # map \edge →
                H.maybe (Graph.getCoordsOfEdge graph edge) \{ px1, px2, py1, py2 } →
                  S.line
                    [ SA.x1 $ 100.0 * px1
                    , SA.y1 $ 100.0 * py1
                    , SA.x2 $ 100.0 * px2
                    , SA.y2 $ 100.0 * py2
                    , H.class_ "dessin-line2"
                    ]
            , edgesOf bestPos # mapWithIndex \i edge →
                H.maybe (Graph.getCoordsOfEdge graph edge) \{ px1, px2, py1, py2 } →
                  S.text
                    [ SA.x $ 50.0 * (px1 + px2)
                    , SA.y $ 50.0 * (py1 + py2) + 2.0
                    , H.class_ "dessin-edge-no"
                    ]
                    [ H.text $ show (i + 1) ]
            ]
        ]
    ]

  customDialog _ = GEditor.view (model ^. _graphEditor) CloseEditor

  rules =
    [ H.text "Le but du jeu est de dessiner le motif indiqué en pointillé en levant le moins souvent possible le crayon."
    , H.br
    , H.text "Pour lever le crayon, tu peux cliquer sur le bouton prévu pour ou utiliser le clic droit."
    ]

  winTitle = "Tu as réussi en " <> show raises <> " levé" <> s