module Main where
import Effect (Effect)
import Prelude
import Pha (app)
import Optic.Core (Lens', lens)
import Lib.Game (init) as LG
import Lib.Random (runRnd)
import Game.Baseball.Model (example, BaseballState)
import Game.Baseball.View (view) as BaseballView
import Game.Nim.Model (NimState, nimState)
import Game.Nim.View (view) as NimView

type RootState = {
    baseball :: BaseballState,
    nim :: NimState
}

mylens :: Lens' RootState NimState
mylens = lens (\r -> r.nim) (\r x -> r{nim = x})

main :: Effect Unit
main = do
    baseballState <- runRnd $ LG.init example
    nimState' <- runRnd $ LG.init nimState
    let state = {
        baseball: baseballState,
        nim: nimState'
    }
    app {
        init: state,
        view: \st -> NimView.view mylens st.nim,
        node: "root"
    }