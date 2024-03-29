module Game.Frog (module M, game) where

import Game.Frog.Model (Model, Msg, update, imodel, onKeyDown) as M
import Game.Frog.View (view) as M
import MamPrelude
import Game.Core (core, CoreMsg(Init))
import Game.Generic (GenericGame)

game ∷ GenericGame M.Model M.Msg
game =
  { init: Just (core Init)
  , view: M.view
  , onKeydown: M.onKeyDown
  }