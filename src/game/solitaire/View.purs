module Game.Solitaire.View where

import MyPrelude
import Lib.Util (coords)
import Pha as H
import Pha.Elements as HH
import Pha.Attributes as P
import Pha.Util (translate)
import Game.Core (PointerPosition, _position, _nbColumns, _nbRows, _pointer, scoreFn)
import Game.Solitaire.Model (State, Msg(..), Board(..), _board, _holes, _dragged, _help)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSelectGroup, iconBestScore, ihelp, iundo, iredo, ireset, irules)
import UI.Template (template, card, bestScoreDialog, gridStyle, incDecGrid, svgCursorStyle, dndBoardProps, dndItemProps)

tricolor ∷ Int → Int → Int → String
tricolor i columns help = 
    case (i `mod` columns + help * (i / columns)) `mod` 3 of
        0 → "red"
        1 → "blue"
        _ → "green"

cursor ∷ ∀a b. PointerPosition → b → H.VDom a
cursor pp _ = HH.circle ([P.r 20.0, H.class_ "solitaire-cursor"] <> svgCursorStyle pp)

view ∷ State → H.VDom Msg
view state = template {config, board, rules, winTitle, scoreDialog} state where
    position = state ^. _position
    columns = state ^. _nbColumns
    rows = state ^. _nbRows
    isCircleBoard = state ^. _board == CircleBoard
    board_ = state ^. _board
    help = state ^. _help
    pointer = state ^. _pointer
    dragged = state ^. _dragged
    holes = state ^. _holes

    itemStyle i = 
        let {row, col} = coords columns i in
        if isCircleBoard then
            translate
                (show $ 125.0 + sin(2.0 * pi * toNumber i / toNumber rows) * 90.0)
                (show $ 125.0 + cos(2.0 * pi * toNumber i / toNumber rows) * 90.0)
        else
            translate
                (show $ 50 * col + 25)
                (show $ 50 * row + 25)

    config =
        let boards = [CircleBoard, Grid3Board, RandomBoard, EnglishBoard, FrenchBoard] in        
        card "Jeu du solitaire"
        [   iconSelectGroup state "Plateau" boards board_ SetBoard \i opt → case i of
                CircleBoard → opt{icon = IconSymbol "#circle", tooltip = Just "Cercle"}
                Grid3Board → opt{icon = IconText "3xN", tooltip = Just "3xN"}
                RandomBoard → opt{icon = IconSymbol "#shuffle", tooltip = Just "Aléatoire"}
                EnglishBoard → opt{icon = IconSymbol "#tea", tooltip = Just "Anglais"}
                FrenchBoard →  opt{icon = IconSymbol "#bread", tooltip = Just "Français"}
        ,   icongroup "Options" $ [ihelp, iundo, iredo, ireset, irules] <#> \x → x state
        ,   iconBestScore state
        ]

    drawHole i = 
        [   H.when (help > 0 && not isCircleBoard) \_ →
                HH.rect
                [   P.x (-25.0)
                ,   P.y (-25.0)
                ,   P.width "50"
                ,   P.height "50"
                ,   H.key $ "rect" <> show i
                ,   P.fill $ tricolor i columns help
                ,   P.transform $ itemStyle i
                ]
        ,   HH.circle (
            [   H.key $ "h" <> show i
            ,   P.r 17.0
            ,   H.class_ "solitaire-hole"
            ,   P.transform $ itemStyle i
            ] <> dndItemProps state 
                {   currentDragged: dragged
                ,   draggable: false
                ,   droppable: true
                ,   id: i
                }
            )
        ]

    drawPeg i =
        HH.circle (
        [   P.r 20.0
        ,   H.key $ "p" <> show i
        ,   H.class_ "solitaire-peg"
        ,   P.transform $ itemStyle i
        ] <> dndItemProps state
            {   draggable: true
            ,   droppable: false
            ,   currentDragged: dragged
            ,   id: i
            }
        )


    grid =
        HH.div (
            [H.class_ "ui-board"] 
            <> dndBoardProps 
            <> (if isCircleBoard then
                    [H.style "width" "100%", H.style "height" "100%"] 
                else 
                    gridStyle rows columns 5
            ))
        [   HH.svg [if isCircleBoard then P.viewBox 0 0 250 250 else P.viewBox 0 0 (50 * columns) (50 * rows)] $ concat
            [   [H.when isCircleBoard \_ →
                    HH.circle [P.cx 125.0, P.cy 125.0, P.r 90.0, H.class_ "solitaire-circle"]
                ]
            ,   concat $ holes # mapWithIndex \i hasHole →
                    if hasHole then drawHole i else []
            ,   position # mapWithIndex \i hasPeg →
                    H.when hasPeg \_ → drawPeg i
            ,   [H.maybeN $ cursor <$> pointer <*> dragged]
            ]
        ]

    board = incDecGrid state [grid]

    scoreDialog _ = bestScoreDialog state \bestPosition → [
        HH.div [H.class_ "ui-flex-center solitaire-scoredialog"] [
            HH.div([H.class_ "ui-board"] <> (if isCircleBoard then 
                                    [H.style "width" "100%", H.style "height" "100%"] 
                                else 
                                    gridStyle rows columns 5
            ))
            [   HH.svg [if isCircleBoard then P.viewBox 0 0 250 250 else P.viewBox 0 0 (50 * columns) (50 * rows)] $ concat
                [   [H.when isCircleBoard \_ →
                        HH.circle 
                        [   P.cx 125.0
                        ,   P.cy 125.0
                        ,   P.r 90.0
                        ,   H.class_ "solitaire-circle"
                        ]
                    ]
                ,   holes # mapWithIndex \i → (_ `H.when` \_ →
                        HH.circle
                        [   H.key $ "h" <> show i
                        ,   P.r 17.0
                        ,   H.class_ "solitaire-hole"
                        ,   P.transform $ itemStyle i
                        ]
                    )
                ,   bestPosition # mapWithIndex \i → (_ `H.when` \_ →
                        HH.circle
                        [   H.key $ "p" <> show i
                        ,   P.r 20.0
                        ,   H.class_ "solitaire-peg"
                        ,   P.transform $ itemStyle i
                        ]
                    )
                ]
            ]
        ]
    ]


    rules = [H.text "Jeu du solitaire", HH.br, H.text "blah blah"]

    nbPegs = scoreFn state
    s = if nbPegs > 1 then "s" else ""
    winTitle = show nbPegs <> " jeton" <> s <> " restant" <> s