module Game.Valise (module M, game) where
import Game.Valise.Model (State, Msg, update, istate, enterA, leaveA) as M
import Game.Valise.View (view) as M
import MyPrelude
import Game.Generic (GenericGame)

game :: GenericGame M.State M.Msg
game = {
    init: Nothing,
    view: M.view,
    onKeydown: const Nothing
}