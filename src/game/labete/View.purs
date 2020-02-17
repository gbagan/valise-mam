module Game.Labete.View (view) where

import MyPrelude
import Lib.Util (coords, map3)
import Math (abs)
import Pha (VDom, Prop, text, (<&&>), maybeN, key, attr, class_, class', style)
import Pha.Elements (div, br)
import Pha.Attributes (href)
import Pha.Events (on', onclick, onpointerdown, onpointerup, onpointerleave)
import Pha.Events.Decoder (shiftKey)
import Pha.Svg (svg, g, rect, use, fill, x_, y_, width, height, transform, viewBox)
import Pha.Util (pc, translate)
import Game.Core (_position, _nbColumns, _nbRows, _pointer, _help, scoreFn)
import Game.Common (pointerDecoder, _isoCustom)
import Game.Labete.Model (State, Msg(..), Mode(..), BeastType(..), nonTrappedBeastOnGrid,
                          _mode, _beast, _beastType, _selectedColor, _startPointer, _squareColors)
import UI.Template (template, card, dialog, bestScoreDialog, incDecGrid, gridStyle, trackPointer, svgCursorStyle)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSelectGroup, iconSizesGroup, iconBestScore, ireset, irules)

colors ∷ Array String
colors = ["#5aa02c", "blue", "red", "yellow", "magenta", "cyan", "orange", "darkgreen", "grey"]

zone ∷ ∀a. Int → {x ∷ Number, y ∷ Number} → {x ∷ Number, y ∷ Number} → VDom a
zone color { x: x1, y: y1 }  {x: x2, y: y2 } =
    rect 
    [   x_ $ pc (min x1 x2)
    ,   y_ $ pc (min y1 y2)
    ,   width $ pc $ abs (x2 - x1)
    ,   height $ pc $ abs (y2 - y1)
    ,   key "zone"
    ,   class_ "labete-zone"
    ,   fill $ colors !! color # fromMaybe ""
    ]

modes ∷ Array Mode
modes = [StandardMode, CylinderMode, TorusMode]

square ∷ ∀a. { color ∷ Int, hasTrap ∷ Boolean, hasBeast ∷ Boolean, row ∷ Int, col ∷ Int} → Array (Prop a) → VDom a
square { color, hasTrap, hasBeast, row, col } props =
    g ([transform $ translate (show $ 50 * col) (show $ 50 * row)] <> props)
    [   use [href "#grass", width "50", height "50", fill $ colors !! color # fromMaybe ""]
    ,   rect [width "51", height "51",  class_ "labete-square-borders"]
    ,   use [href "#paw", x_ "5", y_ "5", width "40", height "40", class_ "labete-beast", class' "visible" hasBeast]
    ,   hasTrap <&&> \_ →
            use [href "#trap", x_ "5", y_ "5", width "40", height "40"]
    ]

ihelp ∷ State → VDom Msg
ihelp state =
    iconbutton
        state {icon: IconSymbol "#help", tooltip: Just "Aide", selected : state^._help}
        [   onpointerdown $ SetHelp true
        ,   onpointerup $ SetHelp false
        ,   onpointerleave $ SetHelp false
        ]

view ∷ State → VDom Msg
view state = template {config, board, rules, winTitle, customDialog, scoreDialog} state where
    rows = state^._nbRows
    columns = state^._nbColumns
    nonTrappedBeast = nonTrappedBeastOnGrid state
    beastTypes = [Type1, Type2, Type3, Type4, CustomBeast]

    config = card "La bête" 
        [   iconSelectGroup state "Forme de la bête" beastTypes (state^._beastType) SetBeast case _ of
                Type1 → _{icon = IconSymbol "#beast1"}
                Type2 → _{icon = IconSymbol "#beast2"}
                Type3 → _{icon = IconSymbol "#beast3"}
                Type4 → _{icon = IconSymbol "#beast23"}
                CustomBeast → _{icon = IconSymbol "#customize"}
        ,   iconSelectGroup state "Type de la grille" modes (state^._mode) SetMode case _ of
                StandardMode → _{icon = IconSymbol "#grid-normal", tooltip = Just "Normale"}
                CylinderMode → _{icon = IconSymbol "#grid-cylinder", tooltip = Just "Cylindrique"}
                TorusMode → _{icon = IconSymbol "#grid-torus", tooltip = Just "Torique"}

        ,   iconSizesGroup state [3∧3, 5∧5, 6∧6] true
        ,   icongroup "Options" $ [ihelp state, ireset state, irules state]
        ,   iconBestScore state
        ]

    cursor pp = use (svgCursorStyle pp <>
        [   key "cursor"
        ,   href "#trap"
        ,   x_ "-20"
        ,   y_ "-20"
        ,   width "40"
        ,   height "40"
        ,   attr "opacity" "0.7" -- todo state.position[state.squareHover] ? 0.3 : 0.7,
        ,   attr "pointer-events" "none"
    ])

    grid = div (gridStyle rows columns 5 <> trackPointer <> [class_ "ui-board",
        on' "pointerdown" $ \ev → shiftKey ev >>= (if _ then
                            Just <$> StartZone2 <$> pointerDecoder ev
                        else
                            pure Nothing
                        )
    ]) [
        svg [viewBox 0 0 (50 * columns) (50 * rows)] (
            (map3 (state^._position) nonTrappedBeast  (state^._squareColors) \index hasTrap hasBeast color →
                let {row, col} = coords columns index in
                square { color, row, col, hasTrap, hasBeast: hasBeast && state^._help } [
                    key $ show index,
                    on' "click" $ shiftKey >>> map (if _ then Nothing else Just (Play index)),
                    -- pointerenter: [actions.setSquareHover, index], todo
                    -- ponterleave: [actions.setSquareHover, null],
                    onpointerup $ FinishZone index,
                    on' "pointerdown" $ shiftKey >>> map (if _ then Just (StartZone index) else Nothing)
                ]
            ) <> [
                maybeN $ case state^._startPointer of
                    Nothing → cursor <$> state^._pointer
                    Just p → zone (state^._selectedColor) p <$> state^._pointer
            ]
        )
    ]

    board = incDecGrid state [
        grid,
        state^._selectedColor > 0 <&&> \_ →
            div [
                class_ "labete-color",
                style "background-color" $ colors !! (state^._selectedColor) # fromMaybe ""
            ][]
    ]

    customDialog _ = dialog "Personnalise ta bête" [
        div [class_ "labete-custombeast-grid-container"] [ 
            svg [viewBox 0 0 250 250] (
                state ^. (_beast ∘ ix 0 ∘ _isoCustom) #
                    mapWithIndex \index hasBeast →
                        let {row, col} = coords 5 index in
                        square {row, col, hasBeast, hasTrap: false, color: 0} [
                            key $ show index,
                            onclick $ FlipCustomBeast index
                        ]
            )
        ]
    ]

    scoreDialog _ = bestScoreDialog state \position → [
        div [class_ "ui-flex-center labete-bestscore-grid-container"] [
            div (gridStyle rows columns 5 <> [class_ "ui-board"])  [
                svg [viewBox 0 0 (50 * columns) (50 * rows)] (
                    position # mapWithIndex \index hasTrap →
                        let {row, col} = coords columns index in
                        square { color: 0, row, col, hasTrap, hasBeast: false } [key $ show index]
                )
            ]
        ]
    ]

    rules = [
        text "Place le moins de pièges possible pour empêcher la bête d'abîmer ta belle pelouse !", br,
        text "Tu peux choisir de jouer avec des bêtes de différentes formes comme celles prédéfinies dans 'Forme de la bête'.", br,
        text "Dans le dernier choix, la bête peut prendre l'une ou l'autre des formes indiquées.", br,
        text "Le plateau de jeu peut prendre une grille, un cylindre ou un tore."
    ]
        
    winTitle = "Record: " <> show (scoreFn state)  <> " pièges"