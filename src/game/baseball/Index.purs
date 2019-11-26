module Game.Baseball (module M, game) where
import Game.Baseball.Model (State, Msg, update, istate) as M
import Game.Baseball.View (view) as M
import MyPrelude
import Game.Core (core, CoreMsg(Init))
import Game.Generic (GenericGame)

game :: GenericGame M.State M.Msg
game = {
    init: Just (core Init),
    view: M.view,
    onKeydown: const Nothing
}