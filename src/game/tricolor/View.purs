module Game.Tricolor.View where

import MamPrelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Game.Core (isLevelFinished, _position)
import Game.Tricolor.Model (Model, Msg(..), _size, _nbColors, _range, _hoverCell, _shuffle, inRange)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (translate)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSelectGroup, iundo, iredo, ireset, irules)
import UI.Template (template, card)

colors ∷ Array String
colors = [ "green", "yellow", "red", "magenta", "blue" ]

translateCell ∷ Int → Int → String
translateCell i size = translate (show x) (show y)
  where
  x = 50.0 + 35.0 * cos (toNumber i * 2.0 * pi / toNumber size)
  y = 45.0 + 35.0 * sin (toNumber i * 2.0 * pi / toNumber size)

irandom ∷ Model → Html Msg
irandom model =
  iconbutton
    model
    { icon: IconSymbol "#shuffle", tooltip: Just "Mélanger", selected: model ^. _shuffle }
    [ E.onClick \_ → Shuffle
    ]

view ∷ Model → Html Msg
view model = template { config, board, rules } model
  where
  position = model ^. _position
  size = model ^. _size
  nbColors = model ^. _nbColors
  range = model ^. _range
  levelFinished = isLevelFinished model
  hoverCell = model ^. _hoverCell

  config =
    card "Feux tricolores"
      [ iconSelectGroup model "Nombre de feux" [ 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 ] size SetSize (const identity)
      , iconSelectGroup model "Nombre de couleurs" [ 2, 3, 4, 5 ] nbColors SetNbColors (const identity)
      , iconSelectGroup model "Portée" [ 1, 2, 3 ] range SetRange (const identity)
      , icongroup "Options" $ [ iundo, iredo, ireset, irandom, irules ] <#> (_ $ model)
      ]

  drawCell i color =
    H.circle
      [ P.r 7.5
      , H.class_ "tricolor-cell"
      , H.class' "finished" levelFinished
      , P.stroke $ if (inRange model i <$> hoverCell) == Just true then "lightgreen" else "black"
      , P.fill $ if levelFinished then "" else colors !! color ?: ""
      , P.transform (translateCell i size)
      , E.onClick \_ → Play i
      , E.onPointerEnter \_ → SetHoverCell (Just i)
      , E.onPointerLeave \_ → SetHoverCell Nothing
      ]

  drawColorCycle =
    ( take nbColors colors # foldMapWithIndex \i color →
        [ H.circle
            [ P.cx $ toNumber (95 + 15 * (i - nbColors))
            , P.cy 95.0
            , P.r 3.0
            , P.fill color
            ]
        , H.path
            [ P.d "M0 2H4V0l3 3l-3 3v-2h-4Z"
            , P.fill "black"
            , P.transform $ translate (show $ 99 + 15 * (i - nbColors)) "92"
            ]
        ]
    ) <> [ H.circle [ P.cx 95.0, P.cy 95.0, P.r 3.0, P.fill "green" ] ]

  board =
    H.div [ H.class_ "ui-board tricolor-board" ]
      [ H.svg [ P.viewBox 0 0 100 100 ]
          [ H.g []
              $ position
              # mapWithIndex drawCell
          , H.g []
              drawColorCycle
          ]
      ]

  rules =
    [ H.text "Ce jeu est une variante de \"Tout noir ou tout blanc\" mais avec plusieurs couleurs."
    , H.br
    , H.text "Lorsque tu cliques un jeton, celui-ci change de couleurs ainsi que tous les jetons proches jusqu'à la distance choisie dans \"Portée\"."
    , H.br
    , H.text "Le but est que tous les jetons soient de couleur verte."
    ]

