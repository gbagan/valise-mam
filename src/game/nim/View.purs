module Game.Nim.View where
import Prelude
import Data.Array (mapWithIndex, concat)
import Data.Tuple (fst, snd)
import Data.Int (toNumber)
import Data.Lens (Lens', (^.))
import Pha (VDom, text)
import Pha.Action ((🎲))
import Pha.Html (div', span, br, svg, rect, use, class', key, style,
            click, width, height, fill, viewBox, translate)
import Lib.Util (tabulate)
import Game.Core (canPlay, playA, isLevelFinished, _position, _turn)
import Game.Nim.Model (NimState, Move(..), _nbPiles, _length, setLengthA, setNbPilesA)
import UI.Template (template, card)
import UI.Icons (icongroup, iconSelectGroup, icons2Players, iundo, iredo, ireset, irules)

view :: forall a. Lens' a NimState -> NimState -> VDom a
view lens state = template lens {config, board, rules, winTitle} state where
    nbPiles = state^._nbPiles
    length = state^._length

    config = card "Poker Nim" [
        iconSelectGroup lens state "Nombre de rangées" [1, 2, 3, 4, 5] (const identity) nbPiles setNbPilesA,
        iconSelectGroup lens state "Taille des rangées" [10, 5] (const identity) length setLengthA,
        icons2Players lens state,
        icongroup "Options" $ [iundo, iredo, ireset, irules] <#> \x -> x lens state
    ]

    board = div' [class' "ui-board nim-board" true] [
        svg [viewBox "0 0 100 100", height "100%", width "100%"] $
            concat $ state^._position # mapWithIndex \i pile ->
                [rect
                    (if length == 5 then 25.0 else 0.0)
                    (toNumber $ 7 + 20 * i)
                    (if length == 5 then 50.0 else 100.0)
                    10.0 [
                        key $ "pile" <> show i,
                        fill "snow"
                    ]
                ] <> (tabulate length \j ->
                    rect (-2.5) (-2.5) 5.0 5.0 [
                        key $ "base-" <> show i <> "-" <> show j,
                        fill "gray",
                        click $ lens 🎲 playA (Move i j),
                        style "transform" $
                            translate (toNumber $ (if length == 5 then 30 else 5) + 10 * j) (toNumber $ 12 + 20 * i) <>
                            " rotate(45deg)",
                        style "cursor" $ if canPlay state (Move i j) then "pointer" else "not-allowed"
                    ]
                ) <> ([fst pile, snd pile] # mapWithIndex \j peg ->
                    use 0.0 0.0 8.0 8.0 "#meeple" [
                        key $ "p-" <> show i <> "-" <> show j,
                        class' "nim-player" true,
                        fill $ if j == 0 then "blue" else "red",
                        style "transform" $ translate (toNumber $ (if length == 5 then 26 else 1) + 10 * peg) (toNumber $ 8 + 20 * i)
                    ]
                ),
        span [class' "nim-turn-message" true] [
            text (if isLevelFinished state then
                "Partie finie"
            else if state^._turn == 0 then
                "Tour du joueur bleu"
            else
                "Tour du second rouge"
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

    winTitle = "Les " <> (if state^._turn == 1 then "bleu" else "rouge") <> "s gagnent"
