module Game.Sansmot (module M, game) where
import Game.Sansmot.Model (State, Msg, update, istate) as M
import Game.Sansmot.View (view) as M
import MyPrelude
import Game.Core (core, CoreMsg(Init))
import Game.Generic (GenericGame)

game :: GenericGame M.State M.Msg
game = {
    init: Nothing,
    view: M.view,
    onKeydown: const Nothing
}