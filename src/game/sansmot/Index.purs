module Game.Sansmot (module M, game) where

import Game.Sansmot.Model (Model, Msg, update, imodel) as M
import Game.Sansmot.View (view) as M
import MamPrelude
import Game.Generic (GenericGame)

game ∷ GenericGame M.Model M.Msg
game =
  { init: Nothing
  , view: M.view
  , onKeydown: const Nothing
  }