module Game.Valise (module M, game) where

import Game.Valise.Model (Model, Msg, update, imodel, enterA) as M
import Game.Valise.View (view) as M
import MamPrelude
import Game.Generic (GenericGame)

game ∷ GenericGame M.Model M.Msg
game =
  { init: Nothing
  , view: M.view
  , onKeydown: const Nothing
  }