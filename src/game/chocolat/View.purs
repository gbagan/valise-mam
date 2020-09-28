module Game.Chocolat.View  (view) where
  
import MyPrelude
import Lib.Util (repeat2)
import Game.Core (_position, _nbRows, _nbColumns, possibleMoves)
import Game.Chocolat.Model (State, Msg(..), Move(..), SoapMode(..), _soap, _soapMode, _moveWhenHover, cutLine) 
import Pha as H
import Pha.Elements as HH
import Pha.Attributes as P
import Pha.Events as E
import Pha.Util (translate, px')
import UI.Template (template, card, gridStyle, incDecGrid, turnMessage, winTitleFor2Players)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, icons2Players, iconSelectGroup, iundo, iredo, ireset, irules)

inside ∷ State → Int → Int → Boolean
inside state row col = col >= left && col <= right - 1 && row >= top && row <= bottom - 1
    where {left, right, top, bottom} = state^._position
    
view ∷ State → H.VDom Msg
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
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> (_ $ state)
        ]

    cutter row col move =
        HH.circle
        [   P.cx $ show (50.0 * toNumber col)
        ,   P.cy $ show (50.0 * toNumber row)
        ,   P.r "7"
        ,   H.key $ "c" <> show (row * (columns + 1) + col)
        ,   H.class_ "chocolat-cutter"
        ,   E.onpointerenter $ SetHover (Just move)
        ,   E.onpointerleave $ SetHover Nothing
        ,   E.onclick $ Play move
        ]

    grid =
        HH.div (gridStyle rows columns 3 <> [H.class_ "ui-board"])
        [   HH.svg [P.viewBox (-7) (-7) (50 * columns + 14) (50 * rows + 14)] $ concat 
            [   repeat2 rows columns \row col →
                HH.rect 
                [   H.key $ "choc" <> show (row * columns + col)
                ,   H.style "transform" $ translate (px' $ 50 * col) (px' $ 50 * row)
                ,   H.class_ "chocolat-square"
                ,   H.class' "soap" $ row == soapRow && col == soapCol
                ,   H.class' "hidden" $ not (inside state row col)
                ]
            ,   possibleMoves state >>= case _ of
                    FromLeft i → [cutter pos.top i (FromLeft i), cutter pos.bottom i (FromLeft i)]
                    FromRight i → [cutter pos.top i (FromRight i), cutter pos.bottom i (FromRight i)]
                    FromTop i → [cutter i pos.left (FromTop i), cutter i pos.right (FromTop i)]
                    FromBottom i → [cutter i pos.left (FromBottom i), cutter i pos.right (FromBottom i)],
                [   HH.use 
                    [   P.href "#skull"
                    ,   P.x $ show (50 * soapCol + 12)
                    ,   P.y $ show (50 * soapRow + 12)
                    ,   P.width "26"
                    ,   P.height "26"
                    ,   H.key "skull"
                    ,   H.class_ "chocolat-skull"
                    ]
                ,   H.maybe (state^._moveWhenHover) \m →
                        let {x1, x2, y1, y2} = cutLine state m in
                        HH.line
                        [   P.x1 $ show (50 * x1)
                        ,   P.y1 $ show (50 * y1)
                        ,   P.x2 $ show (50 * x2)
                        ,   P.y2 $ show (50 * y2)
                        ,   H.key "line"
                        ,   H.class_ "chocolat-cut-line"
                        ]
                ]
            ]
        ]

    board =
        incDecGrid state
        [   grid
        ,   HH.span [H.class_ "frog-turn-message"] [H.text (turnMessage state)]
        ]
            
    rules =
        [   H.text "Chocolat est un jeu à deux joueurs."
        ,   HH.br
        ,   H.text "A chaque tour, un joueur coupe la barre de chocolat en deux et conserve celle qui contient le carré empoisonné."
        ,   HH.br
        ,   H.text "Lorsqu'il ne reste que le carré empoisonné, le joueur qui doit jouer a perdu."
        ]
    winTitle = winTitleFor2Players state
