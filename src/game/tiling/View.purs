module Game.Tiling.View (view) where

import MamPrelude
import Lib.Helpers (coords)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (translate)
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import Game.Helpers (_isoCustom)
import Game.Core (_position, _nbRows, _nbColumns, _pointer, _help)
import Game.Tiling.Model
  ( Model
  , Msg(..)
  , TileType(..)
  , _nbSinks
  , _rotation
  , _tile
  , _tileType
  , sinks
  , needSinks
  , inConflict
  )
import UI.Template (template, card, dialog, incDecGrid, gridStyle, svgCursorStyle, trackPointer)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, iconSelectGroup, ihelp, ireset, irules)

type Borders = { top ∷ Boolean, left ∷ Boolean, bottom ∷ Boolean, right ∷ Boolean }

square ∷ ∀ a. { isDark ∷ Boolean, hasBlock ∷ Boolean, hasSink ∷ Boolean, row ∷ Int, col ∷ Int } → Array (H.Prop a) → Html a
square { isDark, hasBlock, hasSink, row, col } props =
  S.g
    ( [ H.class' "tiling-darken" isDark
      , SA.transform $ translate (50 * col) (50 * row)
      ] <> props
    )
    [ S.rect [ SA.width 50, SA.height 50, SA.fill "url(#concrete)" ]
    , H.when hasBlock \_ →
        S.use [ P.href "#tile2", SA.width 50, SA.height 50 ]
    , H.when hasSink \_ →
        S.use [ P.href "#sink", SA.width 50, SA.height 50 ]
    ]

view ∷ Model → Html Msg
view model = template { config, board, rules, winTitle, customDialog } model
  where
  position = model ^. _position
  rows = model ^. _nbRows
  columns = model ^. _nbColumns
  tileType = model ^. _tileType
  nbSinks = model ^. _nbSinks
  tile = model ^. _tile
  rotation = model ^. _rotation
  help = model ^. _help
  pointer = model ^. _pointer

  border i di = position !! i ≠ position !! (i + di)

  config =
    card "Carrelage"
      [ iconSizesGroup model [ 4 ∧ 5, 5 ∧ 5, 5 ∧ 6, 8 ∧ 8 ] true
      , iconSelectGroup model "Motif de la tuile" [ Type1, Type2, Type3, CustomTile ] tileType SetTile \t →
          _ { icon = IconSymbol ("#" <> show t) }
      , iconSelectGroup model "Nombre d'éviers" [ 0, 1, 2 ] nbSinks SetNbSinks (const identity)
      , icongroup "Options" $ [ ihelp, ireset, irules ] # map (_ $ model)
      ]

  tileCursor pp =
    S.g (svgCursorStyle pp)
      [ S.g
          [ H.class_ "tiling-cursor"
          , H.style "transform" $ "rotate(" <> show (90 * rotation) <> "deg)"
          ] $ tile # map \{ row, col } →
          S.use
            [ P.href "#tile2"
            , SA.x $ 50 * col - 25
            , SA.y $ 50 * row - 25
            , SA.width 50
            , SA.height 50
            , H.attr "pointer-events" "none"
            , SA.opacity (if inConflict model then 0.3 else 0.8)
            ]
      ]

  sinkCursor pp =
    S.use
      ( [ P.href "#sink"
        , SA.x (-25)
        , SA.y (-25)
        , SA.width 50
        , SA.height 50
        , H.attr "pointer-events" "none"
        ] <> svgCursorStyle pp
      )

  grid =
    H.div
      ( gridStyle rows columns 5 <> trackPointer <>
          [ H.class_ "ui-board"
          , E.onContextMenuPrevent \_ → Rotate -- todo prevEff
          ]
      )
      [ S.svg [ SA.viewBox 0.0 0.0 (50.0 * toNumber columns) (50.0 * toNumber rows) ] $ concat
          [ position # mapWithIndex \index pos →
              let
                { row, col } = coords columns index
              in
                square
                  { isDark: help && even (row + col)
                  , hasBlock: pos > 0
                  , hasSink: pos == -1
                  , row
                  , col
                  }
                  [ E.onClick \_ → if needSinks model then PutSink index else Play index
                  , E.onPointerEnter \_ → SetHoverSquare (Just index)
                  , E.onPointerLeave \_ → SetHoverSquare Nothing
                  ]
          , position # mapWithIndex \index pos →
              let
                { row, col } = coords columns index
              in
                S.g [ SA.transform $ translate (show $ 50 * col) (show $ 50 * row) ]
                  [ H.when (pos > 0 && border index (-1)) \_ →
                      S.line [ SA.x1 0.0, SA.y1 0.0, SA.x2 0.0, SA.y2 50.0, SA.stroke "#000", SA.strokeWidth 2.0 ]
                  , H.when (pos > 0 && border index 1) \_ →
                      S.line [ SA.x1 50.0, SA.y1 0.0, SA.x2 50.0, SA.y2 50.0, SA.stroke "#000", SA.strokeWidth 2.0 ]
                  , H.when (pos > 0 && border index (-columns)) \_ →
                      S.line [ SA.x1 0.0, SA.y1 0.0, SA.x2 50.0, SA.y2 0.0, SA.stroke "#000", SA.strokeWidth 2.0 ]
                  , H.when (pos > 0 && border index columns) \_ →
                      S.line [ SA.x1 0.0, SA.y1 50.0, SA.x2 50.0, SA.y2 50.0, SA.stroke "#000", SA.strokeWidth 2.0 ]
                  ]
          , [ H.maybe pointer $ if length (sinks model) < nbSinks then sinkCursor else tileCursor ]
          ]
      ]

  board = incDecGrid model [ grid ]

  customDialog _ = dialog "Personnalise ta tuile"
    [ H.div [ H.class_ "tiling-customtile-grid-container" ]
        [ H.div [ H.class_ "tiling-grid" ]
            [ S.svg [ SA.viewBox 0.0 0.0 250.0 250.0 ]
                ( tile ^. _isoCustom # mapWithIndex \index hasBlock →
                    let
                      { row, col } = coords 5 index
                    in
                      square { hasBlock, row, col, hasSink: false, isDark: false }
                        [ E.onClick \_ → FlipTile index ]
                )
            ]
        ]
    ]

  rules =
    [ H.text "Est-il possible de faire le carrelage de toute votre cuisine, sachant qu'elle peut avoir un ou plusieurs éviers ?"
    , H.br
    , H.text "Tu peux tester avec différentes formes de tuile et différents emplacements d'éviers."
    , H.br
    , H.text "Deux questions sont particulièrement intéressantes:"
    , H.br
    , H.text "- Pour quelles dimensions de la grille et pour quels positions d'éviers peut-on paver une grille avec le premier type de tuiles?"
    , H.br
    , H.text "- Peut-on toujours carreler une grille 8x8 avec les tuiles de type 3 et en posant un évier, et ceci, quelque soit la position de l'évier?"
    ]

  winTitle = "GAGNÉ"

{-
    I.Icon({
                symbol: 'cup',
                tooltip: 'Succès',
                onClick: [actions.showDialog, 'success']

const HelpDialog = () ⇒ C.HelpDialog(
    'Essaie de remplir tout le plateau avec des pavés respectant un certain motif.', br,
    'Utilise le clic droit ou la barre espace pour tourner le pavé', br,
    'Dans les options, tu peux choisir d\'utiliser des éviers.', br,
    'Ceux-ci ne peuvent pas être déplacés et ne peuvent pas être carrelés.'
);

const SuccessDialog = () ⇒
    Dialog({
        title: 'Succès',
        onOk: [actions.showDialog, null]
    },
        div({ class: 'ui-flex-center tiling-success-container' },
            div({
                class: 'tiling-grid',
                style: gridStyle(model.rows, model.columns)
            },
                model.successForThisConf.map(success ⇒
                    Square({
                        hasSink: success,
                        style: {
                            width: 100 / model.columns + '%',
                            height: 100 / model.rows + '%'
                        }
                    })
                )
            )
        )
    );
