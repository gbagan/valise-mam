module Game.Chocolat (module M, game) where
import Game.Chocolat.Model (Model, Msg, update, imodel) as M
import Game.Chocolat.View (view) as V
import MamPrelude
import Game.Core (core, CoreMsg(Init))
import Game.Generic (GenericGame)

game ∷ GenericGame M.Model M.Msg
game = 
    {   init: Just (core Init)
    ,   view: V.view
    ,   onKeydown: const Nothing
    }