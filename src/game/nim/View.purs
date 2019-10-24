module Game.Nim.View where
import Prelude
import Data.Array (mapWithIndex, concat)
import Data.Tuple (fst, snd)
import Optic.Core (Lens', (^.))
import Pha (VDom, text, lensAction)
import Pha.Html (div', span, br, svg, rect, use, class', key, style,
            click, width, href,
            height, x, y, fill, viewBox)
import Lib.Core (repeat)
import Lib.Game ((🎲), canPlay, _play', isLevelFinished, _position, _turn)
import Game.Nim.Model (NimState, Move(..), setNbPiles, _nbPiles, _length, setLength)
import UI.Template (template)
import UI.Dialog (card)
import UI.Icon (icongroup, Icon(IconText))
import UI.Icons (iconbutton, icons2Players, iundo, iredo, ireset, irules)

translate :: Int -> Int -> String
translate x y = "translate(" <> show x <> "px," <> show y <> "px)"

view :: forall a. Lens' a NimState -> NimState -> VDom a
view lens state = template lens elements state where
    -- winTitle: `Les ${state.turn ? 'bleu' : 'rouge'}s gagnent`,
    nbPiles = state^._nbPiles
    length = state^._length

    elements = {
        config: 
            card "Poker Nim" [
                icongroup "Nombre de rangées" $ [1, 2, 3, 4, 5] <#> \i ->
                    iconbutton state (_{
                        icon = IconText $ show i,
                        selected = nbPiles == i
                    }) [click $ lens 🎲 setNbPiles i],
                    icongroup "Taille des rangées" $ [10, 5] <#> \i ->
                        iconbutton state (_{
                            icon = IconText $ show i,
                            selected = length == i
                        }) [click $ lens 🎲 setLength i],

                icons2Players lens state,
                icongroup "Options" $ [iundo, iredo, ireset, irules] <#> \x -> x lens state
            ],

        board:
            div' [class' "ui-board nim-board" true] [
                svg [viewBox "0 0 100 100", height "100%", width "100%"] $
                    concat $ state^._position # mapWithIndex \i pile ->
                        [rect [
                            key $ "pile" <> show i,
                            x $ if length == 5 then "25" else "0",
                            y $ show $ 7 + 20 * i,
                            height "10",
                            width $ if length == 5 then "50" else "100",
                            fill "snow"
                        ] []] <>
                        (repeat length \j ->
                            rect [
                                key $ "base-" <> show i <> "-" <> show j,
                                width "5",
                                height "5",
                                x "-2.5",
                                y "-2.5",
                                fill "gray",
                                click $ lensAction lens $ _play' $ Move i j,
                                style "transform" $ translate ((if length == 5 then 30 else 5) + 10 * j) (12 + 20 * i) <> " rotate(45deg)",
                                style "cursor" $ if canPlay state (Move i j) then "pointer" else "not-allowed"
                            ] []) <> (
                        [fst pile, snd pile] # mapWithIndex \j peg ->
                            use [
                                key $ "p-" <> show i <> "-" <> show j,
                                href "#meeple",
                                width "8",
                                height "8",
                                class' "nim-player" true,
                                fill $ if j == 0 then "blue" else "red",
                                style "transform" $ translate ((if length == 5 then 26 else 1) + 10 * peg) (8 + 20 * i)
                            ] []
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
            ],
        rules: [
            text "Essaie de bloquer ton adversaire", br,
            text "A chaque tour, tu peux déplacer un de tes jetons vers la gauche ou vers la droite", br,
            text "d'autant de cases que tu veux mais tu ne peux pas sauter par dessus le jeton adversaire.", br,
            text "Tu es obligé de déplacer un jeton d'au moins une case, tu ne peux pas passer ton tour.", br,
            text "Tu gagnes la partie si ton adversaire n'a aucun mouvement possible."
        ]
    }
