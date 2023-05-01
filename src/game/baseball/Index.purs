module Game.Baseball (module M, game) where
import MamPrelude
import Game.Baseball.Model (Model, Msg, update, imodel) as M
import Game.Baseball.View (view) as V
import Game.Core (core, CoreMsg(Init))
import Game.Generic (GenericGame)

game ∷ GenericGame M.Model M.Msg
game =
  { init: Just (core Init)
  , view: V.view
  , onKeydown: const Nothing
  }