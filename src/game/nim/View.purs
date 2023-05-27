module Game.Nim.View where

import MamPrelude
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (translate, px')
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import Game.Core (Turn(..), canPlay, isLevelFinished, _position, _turn)
import Game.Nim.Model (Model, Msg(..), Move(..), Position(..), _nbPiles, _length)
import UI.Template (template, card)
import UI.Icons (icongroup, iconSelectGroup, icons2Players, iundo, iredo, ireset, irules)

view ∷ Model → Html Msg
view model = template { config, board, rules, winTitle } model
  where
  position = model ^. _position
  nbPiles = model ^. _nbPiles
  length = model ^. _length
  turn = model ^. _turn

  config =
    card "Bloque moi si tu peux"
      [ iconSelectGroup model "Nombre de rangées" [ 1, 2, 3, 4, 5 ] nbPiles SetNbPiles (const identity)
      , iconSelectGroup model "Taille des rangées" [ 10, 5 ] length SetLength (const identity)
      , icons2Players model
      , icongroup "Options" $ [ iundo, iredo, ireset, irules ] <#> (_ $ model)
      ]

  drawRow i =
    S.rect
      [ H.class_ "nim-row"
      , H.class_ $ if length == 5 then "nim-row-5" else "nim-row-10"
      , SA.y $ toNumber (10 + 19 * i)
      ]

  drawSquare i j =
    S.rect
      [ H.class_ "nim-square"
      , E.onClick \_ → Play (Move i j)
      , H.style "transform"
          $ translate (px' $ (if length == 5 then 30 else 5) + 10 * j) (px' $ 15 + 19 * i)
          <> " rotate(45deg)"
      , H.style "cursor" $ if canPlay model (Move i j) then "pointer" else "not-allowed"
      ]

  drawPeg i player j =
    S.use
      [ P.href "#meeple"
      , SA.width 8
      , SA.height 8
      , H.class_ "nim-player"
      , SA.fill $ if player == 0 then "blue" else "red"
      , H.style "transform" $ translate (px' $ (if length == 5 then 26 else 1) + 10 * j) (px' $ 11 + 19 * i)
      ]

  board =
    H.div [ H.class_ "ui-board nim-board" ]
      [ S.svg [ SA.viewBox 0.0 0.0 100.0 100.0 ]
          ( position # mapWithIndex \i (Position p1 p2) →
              S.g []
                [ drawRow i
                , S.g [] $
                    repeat length (drawSquare i)
                , S.g []
                    $ [ p1, p2 ]
                    # mapWithIndex (drawPeg i)
                ]
          )
      , H.span [ H.class_ "nim-turn-message" ]
          [ H.text
              ( if isLevelFinished model then
                  "Partie finie"
                else if turn == Turn1 then
                  "Tour du joueur bleu"
                else
                  "Tour du joueur rouge"
              )
          ]
      ]

  rules =
    [ H.text "Le but du jeu est d'acculer chacun des jetons de l'adversaire au bord du plateau de telle façon qu'il ne puisse plus en déplacer."
    , H.br
    , H.text "À chaque tour, tu peux déplacer un de tes jetons vers la gauche ou vers la droite d'autant de cases que tu veux mais tu ne peux pas sauter par-dessus un jeton adverse."
    , H.br
    , H.text "Tu es obligé de déplacer un jeton d'au moins une case, tu ne peux pas passer ton tour."
    , H.br
    , H.text "Tu gagnes la partie si ton adversaire n'a aucun mouvement possible."
    ]

  winTitle = "Les " <> (if turn == Turn2 then "bleu" else "rouge") <> "s gagnent"
