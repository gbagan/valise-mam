module Game.Labete.View (view) where

import MamPrelude
import Web.UIEvent.MouseEvent as ME
import Web.PointerEvent.PointerEvent as PE
import Lib.Helpers (coords, map3)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc, translate)
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import Game.Core (_position, _nbColumns, _nbRows, _pointer, _help, scoreFn)
import Game.Helpers (_isoCustom, pointerDecoder)
import Game.Labete.Model
  ( Model
  , Msg(..)
  , Mode(..)
  , BeastType(..)
  , nonTrappedBeastOnGrid
  , _mode
  , _beast
  , _beastType
  , _selectedColor
  , _startPointer
  , _squareColors
  )
import UI.Template (template, card, dialog, bestScoreDialog, incDecGrid, gridStyle, trackPointer, svgCursorStyle)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSelectGroup', iconSizesGroup, iconBestScore, ireset, irules)

colors ∷ Array String
colors = [ "#5aa02c", "blue", "red", "yellow", "magenta", "cyan", "orange", "darkgreen", "grey" ]

zone ∷ ∀ a. Int → { x ∷ Number, y ∷ Number } → { x ∷ Number, y ∷ Number } → Html a
zone color { x: x1, y: y1 } { x: x2, y: y2 } =
  S.rect
    [ SA.x $ pc (min x1 x2)
    , SA.y $ pc (min y1 y2)
    , SA.width $ pc $ abs (x2 - x1)
    , SA.height $ pc $ abs (y2 - y1)
    , H.class_ "labete-zone"
    , SA.fill (colors !! color ?: "")
    ]

square ∷ ∀ a. { color ∷ Int, hasTrap ∷ Boolean, hasBeast ∷ Boolean, row ∷ Int, col ∷ Int } → Array (H.Prop a) → Html a
square { color, hasTrap, hasBeast, row, col } props =
  S.g ([ SA.transform $ translate (show $ 50 * col) (show $ 50 * row) ] <> props)
    [ S.use [ P.href "#grass", SA.width 50, SA.height 50, SA.fill (colors !! color ?: "") ]
    , S.rect [ SA.width 51, SA.height 51, H.class_ "labete-square-borders" ]
    , S.use [ P.href "#paw", SA.x 5.0, SA.y 5.0, SA.width 40, SA.height 40, H.class_ "labete-beast", H.class' "visible" hasBeast ]
    , H.when hasTrap \_ →
        S.use [ P.href "#trap", SA.x 5.0, SA.y 5.0, SA.width "40", SA.height "40" ]
    ]

ihelp ∷ Model → Html Msg
ihelp model =
  iconbutton
    model
    { icon: IconSymbol "#help", tooltip: Just "Aide", selected: model ^. _help }
    [ E.onPointerDown \_ → SetHelp true
    , E.onPointerUp \_ → SetHelp false
    , E.onPointerLeave \_ → SetHelp false
    ]

view ∷ Model → Html Msg
view model = template { config, board, rules, winTitle, customDialog, scoreDialog } model
  where
  rows = model ^. _nbRows
  columns = model ^. _nbColumns
  nonTrappedBeast = nonTrappedBeastOnGrid model

  config = card "La bête"
    [ iconSelectGroup' model "Forme de la bête" (model ^. _beastType) SetBeast
        [ Type1 ∧ _ { icon = IconSymbol "#beast1" }
        , Type2 ∧ _ { icon = IconSymbol "#beast2" }
        , Type3 ∧ _ { icon = IconSymbol "#beast3" }
        , Type4 ∧ _ { icon = IconSymbol "#beast23" }
        , CustomBeast ∧ _ { icon = IconSymbol "#customize" }
        ]
    , iconSelectGroup' model "Type de la grille" (model ^. _mode) SetMode
        [ StandardMode ∧ _ { icon = IconSymbol "#grid-normal", tooltip = Just "Normale" }
        , CylinderMode ∧ _ { icon = IconSymbol "#grid-cylinder", tooltip = Just "Cylindrique" }
        , TorusMode ∧ _ { icon = IconSymbol "#grid-torus", tooltip = Just "Torique" }
        ]
    , iconSizesGroup model [ 3 ∧ 3, 5 ∧ 5, 6 ∧ 6 ] true
    , icongroup "Options" $ [ ihelp, ireset, irules ] # map (_ $ model)
    , iconBestScore model
    ]

  cursor pp = S.use
    ( svgCursorStyle pp <>
        [ P.href "#trap"
        , SA.x (-20)
        , SA.y (-20)
        , SA.width 40
        , SA.height 40
        , SA.opacity 0.7 -- todo model.position[model.squareHover] ? 0.3 : 0.7,
        , H.attr "pointer-events" "none"
        ]
    )

  handlePointerDown ev = do
    let mev = PE.toMouseEvent ev
    if ME.shiftKey mev then do
      pointerDecoder StartZone2 mev
    else
      pure Nothing

  grid = H.div
    ( gridStyle rows columns 5 <> trackPointer <>
        [ H.class_ "ui-board"
        , E.onPointerDown' handlePointerDown
        ]
    )
    [ S.svg [ SA.viewBox 0.0 0.0 (50.0 * toNumber columns) (50.0 * toNumber rows) ]
        [ S.g [] $
            map3 (model ^. _position) nonTrappedBeast (model ^. _squareColors) \index hasTrap hasBeast color →
              let
                { row, col } = coords columns index
              in
                square { color, row, col, hasTrap, hasBeast: hasBeast && model ^. _help }
                  [ E.onClick \ev → if ME.shiftKey ev then NoAction else Play index
                  -- pointerenter: [actions.setSquareHover, index], todo
                  -- ponterleave: [actions.setSquareHover, null],
                  , E.onPointerUp \_ → FinishZone index
                  , E.onPointerDown \ev → if ME.shiftKey (PE.toMouseEvent ev) then StartZone index else NoAction
                  ]
        , H.fromMaybe case model ^. _startPointer of
            Nothing → cursor <$> model ^. _pointer
            Just p → zone (model ^. _selectedColor) p <$> model ^. _pointer
        ]
    ]

  board = incDecGrid model
    [ grid
    , H.when (model ^. _selectedColor > 0) \_ →
        H.div
          [ H.class_ "labete-color"
          , H.style "background-color" (colors !! (model ^. _selectedColor) ?: "")
          ]
          []
    ]

  customDialog _ = dialog "Personnalise ta bête"
    [ H.div [ H.class_ "labete-custombeast-grid-container" ]
        [ S.svg [ SA.viewBox 0.0 0.0 250.0 250.0 ]
            ( model ^. (_beast ∘ ix 0 ∘ _isoCustom) #
                mapWithIndex \index hasBeast →
                  let
                    { row, col } = coords 5 index
                  in
                    square { row, col, hasBeast, hasTrap: false, color: 0 }
                      [ E.onClick \_ → FlipCustomBeast index
                      ]
            )
        ]
    ]

  scoreDialog _ = bestScoreDialog model \position →
    [ H.div [ H.class_ "ui-flex-center labete-bestscore-grid-container" ]
        [ H.div (gridStyle rows columns 5 <> [ H.class_ "ui-board" ])
            [ S.svg [ SA.viewBox 0.0 0.0 (50.0 * toNumber columns) (50.0 * toNumber rows) ]
                ( position # mapWithIndex \index hasTrap →
                    let
                      { row, col } = coords columns index
                    in
                      square { color: 0, row, col, hasTrap, hasBeast: false } []
                )
            ]
        ]
    ]

  rules =
    [ H.text "Place le moins de pièges possible pour empêcher la bête d'abîmer ta belle pelouse !"
    , H.br
    , H.text "Tu peux choisir de jouer avec des bêtes de différentes formes comme celles prédéfinies dans 'Forme de la bête'."
    , H.br
    , H.text "Dans le dernier choix, la bête peut prendre l'une ou l'autre des formes indiquées."
    , H.br
    , H.text "Le plateau de jeu peut prendre une grille, un cylindre ou un tore."
    ]

  winTitle = "Record: " <> show (scoreFn model) <> " pièges"