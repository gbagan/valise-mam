module Game.Labete.View (view) where

import MyPrelude
import Lib.Util (coords, map3)
import Pha as H
import Pha.Elements as HH
import Pha.Attributes as P
import Pha.Events as E
import Pha.Util (pc, translate)
import Web.UIEvent.MouseEvent as ME
import Game.Core (_position, _nbColumns, _nbRows, _pointer, _help, scoreFn)
import Game.Common (pointerDecoder, _isoCustom)
import Game.Labete.Model (State, Msg(..), Mode(..), BeastType(..), nonTrappedBeastOnGrid,
                          _mode, _beast, _beastType, _selectedColor, _startPointer, _squareColors)
import UI.Template (template, card, dialog, bestScoreDialog, incDecGrid, gridStyle, trackPointer, svgCursorStyle)
import UI.Icon (Icon(..))
import UI.Icons (iconbutton, icongroup, iconSelectGroup', iconSizesGroup, iconBestScore, ireset, irules)

colors ∷ Array String
colors = ["#5aa02c", "blue", "red", "yellow", "magenta", "cyan", "orange", "darkgreen", "grey"]

zone ∷ ∀a. Int → {x ∷ Number, y ∷ Number} → {x ∷ Number, y ∷ Number} → H.VDom a
zone color { x: x1, y: y1 }  {x: x2, y: y2 } =
    HH.rect 
    [   H.attr "x" $ pc (min x1 x2)
    ,   H.attr "y" $ pc (min y1 y2)
    ,   P.width $ pc $ abs (x2 - x1)
    ,   P.height $ pc $ abs (y2 - y1)
    ,   H.key "zone"
    ,   H.class_ "labete-zone"
    ,   P.fill $ colors !! color # fromMaybe ""
    ]

square ∷ ∀a. { color ∷ Int, hasTrap ∷ Boolean, hasBeast ∷ Boolean, row ∷ Int, col ∷ Int} → Array (H.Prop a) → H.VDom a
square { color, hasTrap, hasBeast, row, col } props =
    HH.g ([P.transform $ translate (show $ 50 * col) (show $ 50 * row)] <> props)
    [   HH.use [P.href "#grass", P.width "50", P.height "50", P.fill $ colors !! color # fromMaybe ""]
    ,   HH.rect [P.width "51", P.height "51",  H.class_ "labete-square-borders"]
    ,   HH.use [P.href "#paw", P.x 5.0, P.y 5.0, P.width "40", P.height "40", H.class_ "labete-beast", H.class' "visible" hasBeast]
    ,   H.when hasTrap \_ →
            HH.use [P.href "#trap", P.x 5.0, P.y 5.0, P.width "40", P.height "40"]
    ]

ihelp ∷ State → H.VDom Msg
ihelp state =
    iconbutton
        state {icon: IconSymbol "#help", tooltip: Just "Aide", selected: state^._help}
        [   E.onpointerdown $ SetHelp true
        ,   E.onpointerup $ SetHelp false
        ,   E.onpointerleave $ SetHelp false
        ]

view ∷ State → H.VDom Msg
view state = template {config, board, rules, winTitle, customDialog, scoreDialog} state where
    rows = state^._nbRows
    columns = state^._nbColumns
    nonTrappedBeast = nonTrappedBeastOnGrid state
    beastTypes = [Type1, Type2, Type3, Type4, CustomBeast]

    config = card "La bête" 
        [   iconSelectGroup' state "Forme de la bête" (state^._beastType) SetBeast
            [   Type1 ∧ _{icon = IconSymbol "#beast1"}
            ,   Type2 ∧ _{icon = IconSymbol "#beast2"}
            ,   Type3 ∧ _{icon = IconSymbol "#beast3"}
            ,   Type4 ∧ _{icon = IconSymbol "#beast23"}
            ,   CustomBeast ∧ _{icon = IconSymbol "#customize"}
            ]
        ,   iconSelectGroup' state "Type de la grille" (state^._mode) SetMode
            [   StandardMode ∧ _{icon = IconSymbol "#grid-normal", tooltip = Just "Normale"}
            ,   CylinderMode ∧ _{icon = IconSymbol "#grid-cylinder", tooltip = Just "Cylindrique"}
            ,   TorusMode ∧ _{icon = IconSymbol "#grid-torus", tooltip = Just "Torique"}
            ]
        ,   iconSizesGroup state [3∧3, 5∧5, 6∧6] true
        ,   icongroup "Options" $ [ihelp, ireset, irules] <#> (_ $ state)
        ,   iconBestScore state
        ]

    cursor pp = HH.use (svgCursorStyle pp <>
        [   H.key "cursor"
        ,   P.href "#trap"
        ,   P.x (-20.0)
        ,   P.y (-20.0)
        ,   P.width "40"
        ,   P.height "40"
        ,   P.opacity 0.7 -- todo state.position[state.squareHover] ? 0.3 : 0.7,
        ,   H.attr "pointer-events" "none"
    ])

    grid = HH.div (gridStyle rows columns 5 <> trackPointer <> [H.class_ "ui-board",
        E.onpointerdown_ $ \ev → if ME.shiftKey ev then
                            map StartZone2 <$> pointerDecoder (ME.toEvent ev)
                        else
                            pure Nothing
    ]) [
        HH.svg [P.viewBox 0 0 (50 * columns) (50 * rows)] (
            (map3 (state^._position) nonTrappedBeast  (state^._squareColors) \index hasTrap hasBeast color →
                let {row, col} = coords columns index in
                square { color, row, col, hasTrap, hasBeast: hasBeast && state^._help }
                [   H.key $ show index
                ,   E.onclick_ $ \ev → pure $ if ME.shiftKey ev then Nothing else Just (Play index)
                    -- pointerenter: [actions.setSquareHover, index], todo
                    -- ponterleave: [actions.setSquareHover, null],
                ,    E.onpointerup $ FinishZone index
                ,    E.onpointerdown_  $ \ev → pure $ if ME.shiftKey ev then Just (StartZone index) else Nothing
                ]
            ) <> [
                H.maybeN $ case state^._startPointer of
                    Nothing → cursor <$> state^._pointer
                    Just p → zone (state^._selectedColor) p <$> state^._pointer
            ]
        )
    ]

    board = incDecGrid state [
        grid,
        H.when (state^._selectedColor > 0) \_ →
            HH.div [
                H.class_ "labete-color",
                H.style "background-color" $ colors !! (state^._selectedColor) # fromMaybe ""
            ][]
    ]

    customDialog _ = dialog "Personnalise ta bête" [
        HH.div [H.class_ "labete-custombeast-grid-container"] [ 
            HH.svg [P.viewBox 0 0 250 250] (
                state ^. (_beast ∘ ix 0 ∘ _isoCustom) #
                    mapWithIndex \index hasBeast →
                        let {row, col} = coords 5 index in
                        square {row, col, hasBeast, hasTrap: false, color: 0} [
                            H.key $ show index
                        ,   E.onclick $ FlipCustomBeast index
                        ]
            )
        ]
    ]

    scoreDialog _ = bestScoreDialog state \position → [
        HH.div [H.class_ "ui-flex-center labete-bestscore-grid-container"] [
            HH.div (gridStyle rows columns 5 <> [H.class_ "ui-board"])  [
                HH.svg [P.viewBox 0 0 (50 * columns) (50 * rows)] (
                    position # mapWithIndex \index hasTrap →
                        let {row, col} = coords columns index in
                        square { color: 0, row, col, hasTrap, hasBeast: false } [H.key $ show index]
                )
            ]
        ]
    ]

    rules = 
        [   H.text "Place le moins de pièges possible pour empêcher la bête d'abîmer ta belle pelouse !"
        ,   HH.br
        ,   H.text "Tu peux choisir de jouer avec des bêtes de différentes formes comme celles prédéfinies dans 'Forme de la bête'."
        ,   HH.br
        ,   H.text "Dans le dernier choix, la bête peut prendre l'une ou l'autre des formes indiquées."
        ,   HH.br
        ,   H.text "Le plateau de jeu peut prendre une grille, un cylindre ou un tore."
        ]
        
    winTitle = "Record: " <> show (scoreFn state)  <> " pièges"