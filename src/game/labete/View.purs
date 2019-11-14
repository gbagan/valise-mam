module Game.Labete.View where

import MyPrelude
import Lib.Util (coords, map3)
import Math (abs)
import Pha (VDom, Prop, text, ifN, maybeN)
import Pha.Action ((🔍))
import Pha.Event (shiftKey)
import Pha.Html (div', br, svg, g, rect, use, key, attr, class', style, click, pointerdown, pointerup, pointerleave,
                pc, translate, fill, stroke, strokeWidth, transform, viewBox)
import Game.Core (_position, _nbColumns, _nbRows, _pointer, _help, playA, scoreFn)
import Game.Effs (EFFS, getEvent)
import Game.Common (_isoCustom)
import Game.Labete.Model (State, Mode(..), BeastType(..), 
                          _mode, _beast, _beastType, _selectedColor, _startPointer, _squareColors,
                    flipCustomBeastA, nonTrappedBeastOnGrid, setModeA, setHelpA, setBeastA, startZoneA, startZone2A, finishZoneA )
import UI.Template (template, card, dialog, bestScoreDialog, incDecGrid, gridStyle, trackPointer, svgCursorStyle)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSelectGroup, iconSizesGroup, iconBestScore, ireset, irules)

colors :: Array String
colors = ["#5aa02c", "blue", "red", "yellow", "magenta", "cyan", "orange", "darkgreen", "grey"]

zone :: ∀a effs. Int -> {x :: Number, y :: Number} -> {x :: Number, y :: Number} -> VDom a effs
zone color { x: x1, y: y1 }  {x: x2, y: y2 } =
    rect (pc $ min x1 x2) (pc $ min y1 y2) (pc $ abs (x2 - x1)) (pc $ abs (y2 - y1)) [
        key "zone",
        stroke "black",
        fill $ colors !! color # fromMaybe "#5aa02c",
        attr "pointer-events" "none",
        attr "opacity" "0.4"
    ]

modes :: Array Mode
modes = [StandardMode, CylinderMode, TorusMode]

square :: ∀a effs. { color :: Int, hasTrap :: Boolean, hasBeast :: Boolean, row :: Int, col :: Int} -> Array (Prop a effs) -> VDom a effs
square { color, hasTrap, hasBeast, row, col } props =
    g ([transform $ translate (50 * col) (50 * row)] <> props) [
        use 0.0 0.0 50.0 50.0 "#grass" [fill $ colors !! color # fromMaybe "#5aa02c"],
        rect 0.0 0.0 51.0 51.0 [stroke "black", strokeWidth "0.5", fill "transparent"],
        use 5.0 5.0 40.0 40.0 "#paw" [class' "labete-beast" true, class' "visible" hasBeast],
        ifN hasTrap \_ ->
            use 5.0 5.0 40.0 40.0 "#trap" []
    ]

ihelp :: ∀a. Lens' a State -> State -> VDom a EFFS
ihelp lens state =
    iconbutton
        state
        (_{icon = IconSymbol "#help", tooltip = Just "Aide", selected = state^._help}) [
            pointerdown $ lens 🔍 setHelpA true,
            pointerup $ lens 🔍 setHelpA false,
            pointerleave $ lens 🔍 setHelpA false
        ]

view :: ∀a. Lens' a State -> State -> VDom a EFFS
view lens state = template lens (_{config=config, board=board, rules=rules, winTitle=winTitle, 
                                customDialog=customDialog, scoreDialog=scoreDialog}) state where
    rows = state^._nbRows
    columns = state^._nbColumns
    nonTrappedBeast = nonTrappedBeastOnGrid state
    beastTypes = [Type1, Type2, Type3, Type4, CustomBeast]

    config = card "La bête" [
        iconSelectGroup lens state "Forme de la bête" beastTypes (state^._beastType) setBeastA \i opt -> case i of
            Type1 -> opt{icon = IconSymbol "#beast1"}
            Type2 -> opt{icon = IconSymbol "#beast2"}
            Type3 -> opt{icon = IconSymbol "#beast3"}
            Type4 -> opt{icon = IconSymbol "#beast23"}
            CustomBeast -> opt{icon = IconSymbol "#customize"}
        ,
        iconSelectGroup lens state "Type de la grille" modes (state^._mode) setModeA \i opt -> case i of
            StandardMode -> opt{icon = IconSymbol "#grid-normal", tooltip = Just "Normale"}
            CylinderMode -> opt{icon = IconSymbol "#grid-cylinder", tooltip = Just "Cylindrique"}
            TorusMode -> opt{icon = IconSymbol "#grid-torus", tooltip = Just "Torique"},

        iconSizesGroup lens state [3~3, 5~5, 6~6] true,

        icongroup "Options" $ [ihelp, ireset, irules] <#> \x -> x lens state,
        iconBestScore lens state
    ]

    rules = [
        text "Place le moins de pièges possible pour empécher la bête d\'abimer ta belle pelouse!", br,
        text "Tu peux choisir de jouer avec des bêtes de différentes formes comme celles préfinies dans 'Forme de la bête'", br,
        text "Dans le dernier choix, la bête peut prendre soit une ou l\'autre des formes indiquées.", br,
        text "Le plateau de jeu peut prendre une grille, un cylindre ou un tore"
    ]

    cursor pp = use (-20.0) (-20.0) 40.0 40.0 "#trap" (svgCursorStyle pp <> [  
        key "cursor",
        attr "opacity" "0.7", -- state.position[state.squareHover] ? 0.3 : 0.7,
        attr "pointer-events" "none"
    ])

    grid = div' (gridStyle rows columns 5 <> trackPointer lens <> [class' "ui-board" true,
            pointerdown $ lens 🔍 whenM (shiftKey <$> getEvent) startZone2A
    ]) [
        svg [viewBox 0 0 (50 * columns) (50 * rows)] (
            (map3 (state^._position) nonTrappedBeast  (state^._squareColors) \index hasTrap hasBeast color ->
                let {row, col} = coords columns index in
                square { color, row, col, hasTrap, hasBeast: hasBeast && state^._help } [
                    key $ show index,
                    click $ lens 🔍unlessM (shiftKey <$> getEvent) (playA index),
                    -- pointerenter: [actions.setSquareHover, index], todo
                    -- ponterleave: [actions.setSquareHover, null],
                    pointerup $ lens 🔍 finishZoneA index,
                    pointerdown $ lens 🔍 whenM (shiftKey <$> getEvent) (startZoneA index)
                ]
            ) <> [
                maybeN $ case state^._startPointer of
                    Nothing -> cursor <$> state^._pointer
                    Just p -> (zone (state^._selectedColor) p) <$> state^._pointer
            ]
        )
    ]

    board = incDecGrid lens state [
        grid,
        ifN (state^._selectedColor > 0) \_ ->
            div' [
                class' "labete-color" true,
                style "background-color" $ colors !! (state^._selectedColor) # fromMaybe "transparent"
            ][]
    ]

    customDialog _ = dialog lens "Personnalise ta bête" [
        div' [class' "labete-custombeast-grid-container" true] [ 
            svg [viewBox 0 0 250 250] (
                state ^. (_beast ∘ ix 0 ∘ _isoCustom) #
                    mapWithIndex \index hasBeast ->
                        let {row, col} = coords 5 index in
                        square {row, col, hasBeast, hasTrap: false, color: 0} [
                            key $ show index,
                            click $ lens  🔍 flipCustomBeastA index
                        ]
            )
        ]
    ]

    scoreDialog _ = bestScoreDialog lens state \position -> [
        div' [class' "ui-flex-center labete-bestscore-grid-container" true] [
            div' (gridStyle rows columns 5 <> [class' "ui-board" true])  [
                svg [viewBox 0 0 (50 * columns) (50 * rows)] (
                    position # mapWithIndex \index hasTrap ->
                        let {row, col} = coords columns index in
                        square { color: 0, row, col, hasTrap, hasBeast: false } [key $ show index]
                )
            ]
        ]
    ]
        
    winTitle = "Record: " <> show (scoreFn state)  <> " pièges"