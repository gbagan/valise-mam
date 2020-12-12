module Game.Labete (module M, game) where
import Game.Labete.Model (State, Msg, update, istate, onKeyDown) as M
import Game.Labete.View (view) as M
import MyPrelude
import Game.Core (core, CoreMsg(Init))
import Game.Generic (GenericGame)

game ∷ GenericGame M.State M.Msg
game = {
    init: Just (core Init),
    view: M.view,
    onKeydown: M.onKeyDown
}