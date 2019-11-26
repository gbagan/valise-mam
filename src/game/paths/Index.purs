module Game.Paths (module M, game) where
import Game.Paths.Model (State, Msg, update, istate) as M
import Game.Paths.View (view) as M
import MyPrelude
import Game.Core (core, CoreMsg(Init))
import Game.Generic (GenericGame)

game :: GenericGame M.State M.Msg
game = {
    init: Just (core Init),
    view: M.view,
    onKeydown: const Nothing
}