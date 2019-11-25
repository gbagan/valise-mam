module Game.Noirblanc (module M, init) where
import Game.Noirblanc.Model (State, Msg, update, istate) as M
import Game.Noirblanc.View (view) as M
import Pha.Action (Action)
import Game.Effs (EFFS)
import Game.Core (init) as C
init :: Action M.State EFFS
init = C.init