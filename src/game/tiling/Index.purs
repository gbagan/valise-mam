module Game.Tiling (module M, game) where
import Game.Tiling.Model (State, Msg, update, istate, onKeyDown) as M
import Game.Tiling.View (view) as M
import MyPrelude
import Game.Core (core, CoreMsg(Init))
import Game.Generic (GenericGame)

game ∷ GenericGame M.State M.Msg
game = {
    init: Just (core Init),
    view: M.view,
    onKeydown: M.onKeyDown
}