module Game.Nim (module M, game) where
import Game.Nim.Model (Model, Msg, update, imodel) as M
import Game.Nim.View (view) as M
import MamPrelude
import Game.Core (core, CoreMsg(Init))
import Game.Generic (GenericGame)

game ∷ GenericGame M.Model M.Msg
game = {
    init: Just (core Init),
    view: M.view,
    onKeydown: const Nothing
}