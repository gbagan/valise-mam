module Game.Nim.View where
import MyPrelude
import Pha (VDom, text, class_, key, style)
import Pha.Elements (div, span, br)
import Pha.Events ( onclick)
import Pha.Svg (svg, rect, use, fill, viewBox, x_, y_, width, height)
import Pha.Util (translate)
import Lib.Util (tabulate)
import Game.Core (Turn(..), canPlay, isLevelFinished, _position, _turn)
import Game.Nim.Model (State, Msg(..), Move(..), _nbPiles, _length)
import UI.Template (template, card)
import UI.Icons (icongroup, iconSelectGroup, icons2Players, iundo, iredo, ireset, irules)

-- todo
px' :: Int -> String
px' a = show a <> "px"

view :: State -> VDom Msg
view state = template _{config=config, board=board, rules=rules, winTitle=winTitle} state where
    nbPiles = state^._nbPiles
    length = state^._length

    config = card "Poker Nim" [
        iconSelectGroup state "Nombre de rangées" [1, 2, 3, 4, 5] nbPiles SetNbPiles (const identity),
        iconSelectGroup state "Taille des rangées" [10, 5] length SetLength (const identity),
        icons2Players state,
        icongroup "Options" $ [iundo, iredo, ireset, irules] <#> \x -> x state
    ]

    board = div [class_ "ui-board nim-board"] [
        svg [viewBox 0 0 100 100] (
            concat $ state^._position # mapWithIndex \i pile -> concat [
                [rect [
                    x_ $ if length == 5 then "25" else "0",
                    y_ $ show (10 + 19 * i),
                    width $ if length == 5 then "50" else "100",
                    height "10",
                    key $ "pile" <> show i,
                    fill "snow"
                ]],
                tabulate length \j ->
                    rect [
                        x_ "-2.5", y_ "-2.5", width "5", height "5",
                        key $ "base-" <> show i <> "-" <> show j,
                        fill "gray",
                        onclick $ Play (Move i j),
                        style "transform" $
                            translate (px' $ (if length == 5 then 30 else 5) + 10 * j) (px' $ 15 + 19 * i) <>
                            " rotate(45deg)",
                        style "cursor" $ if canPlay state (Move i j) then "pointer" else "not-allowed"
                    ],
                [fst pile, snd pile] # mapWithIndex \j peg ->
                    use "#meeple" [
                        key $ "p-" <> show i <> "-" <> show j,
                        width "8", height "8",
                        class_ "nim-player",
                        fill $ if j == 0 then "blue" else "red",
                        style "transform" $ translate (px' $ (if length == 5 then 26 else 1) + 10 * peg) (px' $ 11 + 19 * i)
                    ]
            ]
        ),
        span [class_ "nim-turn-message"] [
            text (if isLevelFinished state then
                "Partie finie"
            else if state^._turn == Turn1 then
                "Tour du joueur bleu"
            else
                "Tour du joueur rouge"
            )
        ]
    ]
    rules = [
        text "Essaie de bloquer ton adversaire", br,
        text "A chaque tour, tu peux déplacer un de tes jetons vers la gauche ou vers la droite", br,
        text "d'autant de cases que tu veux mais tu ne peux pas sauter par dessus le jeton adversaire.", br,
        text "Tu es obligé de déplacer un jeton d'au moins une case, tu ne peux pas passer ton tour.", br,
        text "Tu gagnes la partie si ton adversaire n'a aucun mouvement possible."
    ]

    winTitle = "Les " <> (if state^._turn == Turn2 then "bleu" else "rouge") <> "s gagnent"
