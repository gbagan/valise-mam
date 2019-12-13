module Game.Nim.View where
import MyPrelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Pha (VDom, text, class_, key, style)
import Pha.Elements (div, span, br)
import Pha.Events ( onclick)
import Pha.Svg (svg, rect, use, fill, viewBox, x_, y_, width, height)
import Pha.Util (translate, px')
import Lib.Util (tabulate)
import Game.Core (Turn(..), canPlay, isLevelFinished, _position, _turn)
import Game.Nim.Model (State, Msg(..), Move(..), _nbPiles, _length)
import UI.Template (template, card)
import UI.Icons (icongroup, iconSelectGroup, icons2Players, iundo, iredo, ireset, irules)

view ∷ State → VDom Msg
view state = template {config, board, rules, winTitle} state where
    nbPiles = state^._nbPiles
    length = state^._length

    config =
        card "Poker Nim"
        [   iconSelectGroup state "Nombre de rangées" [1, 2, 3, 4, 5] nbPiles SetNbPiles (const identity)
        ,   iconSelectGroup state "Taille des rangées" [10, 5] length SetLength (const identity)
        ,   icons2Players state
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> \x → x state
        ]

    drawRow i =
        rect
        [   key $ "pile" <> show i
        ,   class_ "nim-row"
        ,   class_ $ if length == 5 then "nim-row-5" else "nim-row-10"
        ,   y_ $ show (10 + 19 * i)
        ]

    drawSquare i j =
        rect
        [   key $ "base-" <> show i <> "-" <> show j
        ,   class_ "nim-square"
        ,   onclick $ Play (Move i j)
        ,   style "transform" $
                    translate (px' $ (if length == 5 then 30 else 5) + 10 * j) (px' $ 15 + 19 * i)
                    <> " rotate(45deg)"
        ,   style "cursor" $ if canPlay state (Move i j) then "pointer" else "not-allowed"
        ]

    drawPeg i player j =
        use "#meeple"
        [   key $ "p-" <> show i <> "-" <> show player
        ,   width "8"
        ,   height "8"
        ,   class_ "nim-player"
        ,   fill $ if player == 0 then "blue" else "red"
        ,   style "transform" $ translate (px' $ (if length == 5 then 26 else 1) + 10 * j) (px' $ 11 + 19 * i)
        ]

    board =
        div [class_ "ui-board nim-board"]
        [   svg [viewBox 0 0 100 100] (
                state^._position # foldMapWithIndex \i (p1 /\ p2) → concat
                    [   [drawRow i]
                    ,   tabulate length (drawSquare i)
                    ,   [p1, p2] # mapWithIndex (drawPeg i)
                    ]
            )
        ,   span [class_ "nim-turn-message"] [
            text (
                if isLevelFinished state then
                    "Partie finie"
                    else if state^._turn == Turn1 then
                    "Tour du joueur bleu"
                else
                    "Tour du joueur rouge"
                )
            ]
        ]

    rules = 
        [   text "Le but du jeu est d'acculer chacun des jetons de l'adversaire au bord du plateau de telle façon qu'il ne puisse plus en déplacer."
        ,   br
        ,   text "À chaque tour, tu peux déplacer un de tes jetons vers la gauche ou vers la droite d'autant de cases que tu veux mais tu ne peux pas sauter par-dessus un jeton adverse."
        ,   br
        ,   text "Tu es obligé de déplacer un jeton d'au moins une case, tu ne peux pas passer ton tour."
        ,   br
        ,   text "Tu gagnes la partie si ton adversaire n'a aucun mouvement possible."
        ]

    winTitle = "Les " <> (if state^._turn == Turn2 then "bleu" else "rouge") <> "s gagnent"
