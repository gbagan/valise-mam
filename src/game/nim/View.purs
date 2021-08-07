module Game.Nim.View where
import MyPrelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Keyed as K
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (translate, px')
import Game.Core (Turn(..), canPlay, isLevelFinished, _position, _turn)
import Game.Nim.Model (State, Msg(..), Move(..), Position(..), _nbPiles, _length)
import UI.Template (template, card)
import UI.Icons (icongroup, iconSelectGroup, icons2Players, iundo, iredo, ireset, irules)

view ∷ State → Html Msg
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
        ("pile" <> show i) /\ H.rect
        [   H.class_ "nim-row"
        ,   H.class_ $ if length == 5 then "nim-row-5" else "nim-row-10"
        ,   P.y $ toNumber (10 + 19 * i)
        ]

    drawSquare i j =
        ("s-" <> show i <> "-" <> show j) /\ H.rect
        [   H.class_ "nim-square"
        ,   E.onClick \_ -> Play (Move i j)
        ,   H.style "transform" $
                    translate (px' $ (if length == 5 then 30 else 5) + 10 * j) (px' $ 15 + 19 * i)
                    <> " rotate(45deg)"
        ,   H.style "cursor" $ if canPlay state (Move i j) then "pointer" else "not-allowed"
        ]

    drawPeg i player j =
        ("p-" <> show i <> "-" <> show player) /\ H.use 
        [   P.href "#meeple"
        ,   P.width "8"
        ,   P.height "8"
        ,   H.class_ "nim-player"
        ,   P.fill $ if player == 0 then "blue" else "red"
        ,   H.style "transform" $ translate (px' $ (if length == 5 then 26 else 1) + 10 * j) (px' $ 11 + 19 * i)
        ]

    board =
        H.div [H.class_ "ui-board nim-board"]
        [   K.svg [P.viewBox 0 0 100 100] (
                position # foldMapWithIndex \i (Position p1 p2) → concat
                    [   [drawRow i]
                    ,   repeat length (drawSquare i)
                    ,   [p1, p2] # mapWithIndex (drawPeg i)
                    ]
            )
        ,   H.span [H.class_ "nim-turn-message"] [
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
        ,   H.br
        ,   H.text "À chaque tour, tu peux déplacer un de tes jetons vers la gauche ou vers la droite d'autant de cases que tu veux mais tu ne peux pas sauter par-dessus un jeton adverse."
        ,   H.br
        ,   H.text "Tu es obligé de déplacer un jeton d'au moins une case, tu ne peux pas passer ton tour."
        ,   H.br
        ,   H.text "Tu gagnes la partie si ton adversaire n'a aucun mouvement possible."
        ]

    winTitle = "Les " <> (if turn == Turn2 then "bleu" else "rouge") <> "s gagnent"
