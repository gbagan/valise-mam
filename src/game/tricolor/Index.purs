module Game.Tricolor (module M, game) where
import Game.Tricolor.Model (State, Msg, update, istate) as M
import Game.Tricolor.View (view) as M
import MamPrelude
import Game.Core (core, CoreMsg(Init))
import Game.Generic (GenericGame)

game ∷ GenericGame M.State M.Msg
game = {
    init: Just (core Init),
    view: M.view,
    onKeydown: const Nothing
}