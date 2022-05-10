module Game.Eternal (module M, game) where
import Game.Eternal.Model (State, Msg, update, istate) as M
import Game.Eternal.View (view) as M
import MamPrelude
import Game.Core (core, CoreMsg(Init))
import Game.Generic (GenericGame)

game ∷ GenericGame M.State M.Msg
game = {
    init: Just (core Init),
    view: M.view,
    onKeydown: const Nothing
}