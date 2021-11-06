module Game.Chocolat.View  (view) where
  
import MyPrelude
import Game.Chocolat.Model (State, Msg(..), Move(..), SoapMode(..), _soap, _soapMode, _moveWhenHover, cutLine)
import Game.Core (_position, _nbRows, _nbColumns, _pointer, possibleMoves, PointerPosition)
import Lib.Util (repeat2)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (translate)
import UI.Icon (Icon(..))
import UI.Icons (icongroup, iconSizesGroup, icons2Players, iconSelectGroup', iundo, iredo, ireset, irules)
import UI.Template (template, card, gridStyle, incDecGrid, turnMessage, winTitleFor2Players, svgCursorStyle, trackPointer)

inside ∷ State → Int → Int → Boolean
inside state row col = col >= left && col <= right - 1 && row >= top && row <= bottom - 1
    where {left, right, top, bottom} = state^._position

soapCursor ∷ ∀a. PointerPosition → Html a
soapCursor pp =
    H.use $
    [   P.href "#skull"
    ,   H.class_ "paths-cursor"
    ,   P.x (-20.0)
    ,   P.y (-20.0)
    ,   P.width "26"
    ,   P.height "26"
    ] <> svgCursorStyle pp
    
view ∷ State → Html Msg
view state = template {config, board, rules, winTitle} state where
    pos = state^._position
    rows = state^._nbRows
    columns = state^._nbColumns
    soap = state^._soap
    pointer = state^._pointer

    config =
        card "Barre de chocolat"
        [   iconSizesGroup state [6∧7] true
        ,   iconSelectGroup' state "Emplacement du savon" (state^._soapMode) SetSoapMode
            [   CornerMode ∧ _{icon = IconSymbol "#choc-mode0", tooltip = Just "Dans le coin"}
            ,   BorderMode ∧ _{icon = IconSymbol "#choc-mode1", tooltip = Just "Sur un bord"}
            ,   StandardMode ∧ _{icon = IconSymbol "#choc-mode2", tooltip = Just "N'importe où"}
            ,   CustomMode ∧ _{icon = IconSymbol "#customize", tooltip = Just "Personnalisé"}
            ]
        ,   icons2Players state
        ,   icongroup "Options" $ [iundo, iredo, ireset, irules] <#> (_ $ state)
        ]

    cutter row col move =
        H.circle
        [   P.cx $ 50.0 * toNumber col
        ,   P.cy $ 50.0 * toNumber row
        ,   P.r 7.0
        ,   H.class_ "chocolat-cutter"
        ,   E.onPointerEnter \_ → SetHover (Just move)
        ,   E.onPointerLeave \_ → SetHover Nothing
        ,   E.onClick \_ → Play move
        ]

    grid =
        H.div (gridStyle rows columns 3 <> trackPointer <> [H.class_ "ui-board"])
        [   H.svg [P.viewBox (-7) (-7) (50 * columns + 14) (50 * rows + 14)]
            [   H.g [] $
                    repeat2 rows columns \row col →
                        H.rect 
                        [   P.transform $ translate (show $ 50 * col) (show $ 50 * row)
                        ,   H.class_ "chocolat-square"
                        ,   H.class' "soap" $ state^._soap # maybe false \p → {row, col} == p
                        ,   H.class' "hidden" $ not (inside state row col)
                        ,   E.onClick \_ → if isNothing soap then SetSoap row col else NoAction
                        ]
            ,   H.g [] $
                    possibleMoves state >>= case _ of
                        FromLeft i → [cutter pos.top i (FromLeft i), cutter pos.bottom i (FromLeft i)]
                        FromRight i → [cutter pos.top i (FromRight i), cutter pos.bottom i (FromRight i)]
                        FromTop i → [cutter i pos.left (FromTop i), cutter i pos.right (FromTop i)]
                        FromBottom i → [cutter i pos.left (FromBottom i), cutter i pos.right (FromBottom i)]
            ,   H.maybe soap \{row, col} → 
                    H.use 
                    [   P.href "#skull"
                    ,   P.x $ toNumber (50 * col + 12)
                    ,   P.y $ toNumber (50 * row + 12)
                    ,   P.width "26"
                    ,   P.height "26"
                    ,   H.class_ "chocolat-skull"
                    ]
            ,   H.maybe (state^._moveWhenHover) \m →
                    let {x1, x2, y1, y2} = cutLine state m in
                    H.line
                    [   P.x1 $ toNumber (50 * x1)
                    ,   P.y1 $ toNumber (50 * y1)
                    ,   P.x2 $ toNumber (50 * x2)
                    ,   P.y2 $ toNumber (50 * y2)
                    ,   H.class_ "chocolat-cut-line"
                    ]
            ,   H.maybe pointer \pp →
                    if isNothing soap then
                        soapCursor pp
                    else
                        H.empty
            ]
        ]

    board =
        incDecGrid state
        [   grid
        ,   H.span [H.class_ "frog-turn-message"] [H.text $ 
                                                    if isNothing soap then
                                                        "Place le savon"
                                                    else
                                                        turnMessage state
                                                    ]
        ]
            
    rules =
        [   H.text "Chocolat est un jeu à deux joueurs."
        ,   H.br
        ,   H.text "A chaque tour, un joueur coupe la barre de chocolat en deux et conserve celle qui contient le carré empoisonné."
        ,   H.br
        ,   H.text "Lorsqu'il ne reste que le carré empoisonné, le joueur qui doit jouer a perdu."
        ]
    winTitle = winTitleFor2Players state
