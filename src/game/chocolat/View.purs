module Game.Chocolat.View  (view) where
  
import MyPrelude
import Lib.Util (repeat2)
import Game.Core (_position, _nbRows, _nbColumns, possibleMoves)
import Game.Chocolat.Model (State, Msg(..), Move(..), SoapMode(..), _soap, _soapMode, _moveWhenHover, cutLine) 
import Pha (VDom, text, (<??>), key, class_, class', style)
import Pha.Elements (div, span, br)
import Pha.Attributes (href)
import Pha.Events (onclick, onpointerenter, onpointerleave)
import Pha.Svg (svg, rect, line, circle, use, viewBox, width, height, x_, y_, cx, cy, r_, x1_, x2_, y1_, y2_)
import Pha.Util (translate, px')
import UI.Template (template, card, gridStyle, incDecGrid, turnMessage, winTitleFor2Players)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, icons2Players, iconSelectGroup, iundo, iredo, ireset, irules)

inside ∷ State → Int → Int → Boolean
inside state row col = col >= left && col <= right - 1 && row >= top && row <= bottom - 1
    where {left, right, top, bottom} = state^._position
    
view ∷ State → VDom Msg
view state = template {config, board, rules, winTitle} state where
    pos = state^._position
    rows = state^._nbRows
    columns = state^._nbColumns
    {col: soapCol, row: soapRow} = state^._soap

    config =
        card "Barre de chocolat"
        [   iconSizesGroup state [6∧7] true
        ,   iconSelectGroup state "Emplacement du savon" [CornerMode, BorderMode, StandardMode] 
                (state^._soapMode) SetSoapMode \mode opt → case mode of
                    CornerMode → opt{icon = IconSymbol "#choc-mode0", tooltip = Just "Dans le coin"}
                    BorderMode → opt{icon = IconSymbol "#choc-mode1", tooltip = Just "Sur un bord"}
                    StandardMode → opt{icon = IconSymbol "#choc-mode2", tooltip = Just "N'importe où"}
        ,   icons2Players state
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> \x → x state
        ]

    cutter row col move =
        circle
        [   cx $ show (50.0 * toNumber col)
        ,   cy $ show (50.0 * toNumber row)
        ,   r_ "7"
        ,   key $ "c" <> show (row * (columns + 1) + col)
        ,   class_ "chocolat-cutter"
        ,   onpointerenter $ SetHover (Just move)
        ,   onpointerleave $ SetHover Nothing
        ,   onclick $ Play move
        ]

    grid =
        div (gridStyle rows columns 3 <> [class_ "ui-board"])
        [   svg [viewBox (-7) (-7) (50 * columns + 14) (50 * rows + 14)] $ concat 
            [   repeat2 rows columns \row col →
                rect 
                [   key $ "choc" <> show (row * columns + col)
                ,   style "transform" $ translate (px' $ 50 * col) (px' $ 50 * row)
                ,   class_ "chocolat-square"
                ,   class' "soap" $ row == soapRow && col == soapCol
                ,   class' "hidden" $ not (inside state row col)
                ]
            ,   possibleMoves state >>= case _ of
                    FromLeft i → [cutter pos.top i (FromLeft i), cutter pos.bottom i (FromLeft i)]
                    FromRight i → [cutter pos.top i (FromRight i), cutter pos.bottom i (FromRight i)]
                    FromTop i → [cutter i pos.left (FromTop i), cutter i pos.right (FromTop i)]
                    FromBottom i → [cutter i pos.left (FromBottom i), cutter i pos.right (FromBottom i)],
                [   use 
                    [   href "#skull"
                    ,   x_ $ show (50 * soapCol + 12)
                    ,   y_ $ show (50 * soapRow + 12)
                    ,   width "26"
                    ,   height "26"
                    ,   key "skull"
                    ,   class_ "chocolat-skull"
                    ]
                ,   state^._moveWhenHover <??> \m →
                        let {x1, x2, y1, y2} = cutLine state m in
                        line
                        [   x1_ $ show (50 * x1)
                        ,   y1_ $ show (50 * y1)
                        ,   x2_ $ show (50 * x2)
                        ,   y2_ $ show (50 * y2)
                        ,   key "line"
                        ,   class_ "chocolat-cut-line"
                        ]
                ]
            ]
        ]

    board =
        incDecGrid state
        [   grid
        ,   span [class_ "frog-turn-message"] [text (turnMessage state)]
        ]
            
    rules =
        [   text "Chocolat est un jeu à deux joueurs."
        ,   br
        ,   text "A chaque tour, un joueur coupe la barre de chocolat en deux et conserve celle qui contient le carré empoisonné."
        ,   br
        ,   text "Lorsqu'il ne reste que le carré empoisonné, le joueur qui doit jouer a perdu."
        ]
    winTitle = winTitleFor2Players state
