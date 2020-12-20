module Game.Nim.View where
import MyPrelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Pha as H
import Pha.Elements as HH
import Pha.Attributes as P
import Pha.Events as E
import Pha.Util (translate, px')
import Lib.Util (repeat)
import Game.Core (Turn(..), canPlay, isLevelFinished, _position, _turn)
import Game.Nim.Model (State, Msg(..), Move(..), Position(..), _nbPiles, _length)
import UI.Template (template, card)
import UI.Icons (icongroup, iconSelectGroup, icons2Players, iundo, iredo, ireset, irules)

view ∷ State → H.VDom Msg
view state = template {config, board, rules, winTitle} state where
    position = state ^. _position
    nbPiles = state ^. _nbPiles
    length = state ^. _length
    turn = state ^. _turn

    config =
        card "Bloque moi si tu peux"
        [   iconSelectGroup state "Nombre de rangées" [1, 2, 3, 4, 5] nbPiles SetNbPiles (const identity)
        ,   iconSelectGroup state "Taille des rangées" [10, 5] length SetLength (const identity)
        ,   icons2Players state
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> (_ $ state)
        ]

    drawRow i =
        HH.rect
        [   H.key $ "pile" <> show i
        ,   H.class_ "nim-row"
        ,   H.class_ $ if length == 5 then "nim-row-5" else "nim-row-10"
        ,   P.y $ toNumber (10 + 19 * i)
        ]

    drawSquare i j =
        HH.rect
        [   H.key $ "base-" <> show i <> "-" <> show j
        ,   H.class_ "nim-square"
        ,   E.onclick $ Play (Move i j)
        ,   H.style "transform" $
                    translate (px' $ (if length == 5 then 30 else 5) + 10 * j) (px' $ 15 + 19 * i)
                    <> " rotate(45deg)"
        ,   H.style "cursor" $ if canPlay state (Move i j) then "pointer" else "not-allowed"
        ]

    drawPeg i player j =
        HH.use 
        [   P.href "#meeple"
        ,   H.key $ "p-" <> show i <> "-" <> show player
        ,   P.width "8"
        ,   P.height "8"
        ,   H.class_ "nim-player"
        ,   P.fill $ if player == 0 then "blue" else "red"
        ,   H.style "transform" $ translate (px' $ (if length == 5 then 26 else 1) + 10 * j) (px' $ 11 + 19 * i)
        ]

    board =
        HH.div [H.class_ "ui-board nim-board"]
        [   HH.svg [P.viewBox 0 0 100 100] (
                position # foldMapWithIndex \i (Position p1 p2) → concat
                    [   [drawRow i]
                    ,   repeat length (drawSquare i)
                    ,   [p1, p2] # mapWithIndex (drawPeg i)
                    ]
            )
        ,   HH.span [H.class_ "nim-turn-message"] [
            H.text (
                if isLevelFinished state then
                    "Partie finie"
                    else if turn == Turn1 then
                    "Tour du joueur bleu"
                else
                    "Tour du joueur rouge"
                )
            ]
        ]

    rules = 
        [   H.text "Le but du jeu est d'acculer chacun des jetons de l'adversaire au bord du plateau de telle façon qu'il ne puisse plus en déplacer."
        ,   HH.br
        ,   H.text "À chaque tour, tu peux déplacer un de tes jetons vers la gauche ou vers la droite d'autant de cases que tu veux mais tu ne peux pas sauter par-dessus un jeton adverse."
        ,   HH.br
        ,   H.text "Tu es obligé de déplacer un jeton d'au moins une case, tu ne peux pas passer ton tour."
        ,   HH.br
        ,   H.text "Tu gagnes la partie si ton adversaire n'a aucun mouvement possible."
        ]

    winTitle = "Les " <> (if turn == Turn2 then "bleu" else "rouge") <> "s gagnent"
