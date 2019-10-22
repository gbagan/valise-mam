module Main where
import Effect (Effect)
import Prelude
import Pha (app)
import Optic.Core (Lens', lens)
import Lib.Game (init) as LG
import Lib.Random (runRnd)
import Game.Baseball.Model (example, BaseballState)
import Game.Baseball.View (view)

type RootState = {
    baseball :: BaseballState
}

mylens :: Lens' RootState BaseballState
mylens = lens (\r -> r.baseball) (\r x -> r{baseball = x})

main :: Effect Unit
main = do
    baseballState <- runRnd $ LG.init example
    let state = {
        baseball: baseballState
    }
    app {
        init: state,
        view: \st -> view mylens st.baseball,
        node: "root"
    }