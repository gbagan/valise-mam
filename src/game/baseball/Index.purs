module Game.Baseball (module M, game) where
import MyPrelude
import Game.Baseball.Model (State, Msg, update, istate) as M
import Game.Baseball.View (view) as V
import Game.Core (core, CoreMsg(Init))
import Game.Generic (GenericGame)

game ∷ GenericGame M.State M.Msg
game =
    {   init: Just (core Init)
    ,   view: V.view
    ,   onKeydown: const Nothing
    }