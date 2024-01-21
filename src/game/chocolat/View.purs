module Game.Chocolat.View (view) where

import MamPrelude
import Game.Chocolat.Model (Model, Msg(..), Move(..), SoapMode(..), _soap, _soapMode, _moveWhenHover, cutLine)
import Game.Core (_position, _nbRows, _nbColumns, _pointer, possibleMoves, PointerPosition)
import Lib.Helpers (repeat2)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (translate)
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, icons2Players, iconSelectGroup', iundo, iredo, ireset, irules)
import UI.Template (template, card, gridStyle, incDecGrid, turnMessage, winTitleFor2Players, svgCursorStyle, trackPointer)

inside ∷ Model → Int → Int → Boolean
inside model row col = col >= left && col <= right - 1 && row >= top && row <= bottom - 1
  where
  { left, right, top, bottom } = model ^. _position

soapCursor ∷ ∀ a. PointerPosition → Html a
soapCursor pp =
  S.use
    $
      [ P.href "#skull"
      , H.class_ "paths-cursor"
      , SA.x (-20.0)
      , SA.y (-20.0)
      , SA.width 26
      , SA.height 26
      ]
    <> svgCursorStyle pp

view ∷ Model → Html Msg
view model = template { config, board, rules, winTitle } model
  where
  pos = model ^. _position
  rows = model ^. _nbRows
  columns = model ^. _nbColumns
  soap = model ^. _soap
  pointer = model ^. _pointer

  config =
    card "Barre de chocolat"
      [ iconSizesGroup model [ 6 ∧ 7 ] true
      , iconSelectGroup' model "Emplacement du savon" (model ^. _soapMode) SetSoapMode
          [ CornerMode ∧ _ { icon = IconSymbol "#choc-mode0", tooltip = Just "Dans le coin" }
          , BorderMode ∧ _ { icon = IconSymbol "#choc-mode1", tooltip = Just "Sur un bord" }
          , StandardMode ∧ _ { icon = IconSymbol "#choc-mode2", tooltip = Just "N'importe où" }
          , CustomMode ∧ _ { icon = IconSymbol "#customize", tooltip = Just "Personnalisé" }
          ]
      , icons2Players model
      , icongroup "Options" $ [ iundo, iredo, ireset, irules ] # map (_ $ model)
      ]

  cutter row col move =
    S.circle
      [ SA.cx $ 50.0 * toNumber col
      , SA.cy $ 50.0 * toNumber row
      , SA.r 7.0
      , H.class_ "chocolat-cutter"
      , E.onPointerEnter \_ → SetHover (Just move)
      , E.onPointerLeave \_ → SetHover Nothing
      , E.onClick \_ → Play move
      ]

  grid =
    H.div (gridStyle rows columns 3 <> trackPointer <> [ H.class_ "ui-board" ])
      [ S.svg [ SA.viewBox (-7.0) (-7.0) (50.0 * toNumber columns + 14.0) (50.0 * toNumber rows + 14.0) ]
          [ S.g [] $
              repeat2 rows columns \row col →
                S.rect
                  [ SA.transform $ translate (show $ 50 * col) (show $ 50 * row)
                  , H.class_ "chocolat-square"
                  , H.class' "soap" $ model ^. _soap # maybe false \p → { row, col } == p
                  , H.class' "hidden" $ not (inside model row col)
                  , E.onClick \_ → if isNothing soap then SetSoap row col else NoAction
                  ]
          , S.g []
              $ possibleMoves model
              >>= case _ of
                FromLeft i → [ cutter pos.top i (FromLeft i), cutter pos.bottom i (FromLeft i) ]
                FromRight i → [ cutter pos.top i (FromRight i), cutter pos.bottom i (FromRight i) ]
                FromTop i → [ cutter i pos.left (FromTop i), cutter i pos.right (FromTop i) ]
                FromBottom i → [ cutter i pos.left (FromBottom i), cutter i pos.right (FromBottom i) ]
          , H.maybe soap \{ row, col } →
              S.use
                [ P.href "#skull"
                , SA.x $ 50 * col + 12
                , SA.y $ 50 * row + 12
                , SA.width 26
                , SA.height 26
                , H.class_ "chocolat-skull"
                ]
          , H.maybe (model ^. _moveWhenHover) \m →
              let
                { x1, x2, y1, y2 } = cutLine model m
              in
                S.line
                  [ SA.x1 $ 50 * x1
                  , SA.y1 $ 50 * y1
                  , SA.x2 $ 50 * x2
                  , SA.y2 $ 50 * y2
                  , H.class_ "chocolat-cut-line"
                  ]
          , H.maybe pointer \pp →
              if isNothing soap then
                soapCursor pp
              else
                H.empty
          ]
      ]

  board =
    incDecGrid model
      [ grid
      , H.span [ H.class_ "frog-turn-message" ]
          [ H.text $
              if isNothing soap then
                "Place le savon"
              else
                turnMessage model
          ]
      ]

  rules =
    [ H.text "Chocolat est un jeu à deux joueurs."
    , H.br
    , H.text "A chaque tour, un joueur coupe la barre de chocolat en deux et conserve celle qui contient le carré empoisonné."
    , H.br
    , H.text "Lorsqu'il ne reste que le carré empoisonné, le joueur qui doit jouer a perdu."
    ]
  winTitle = winTitleFor2Players model
