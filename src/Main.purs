module Main where
import Prelude
import Effect (Effect)
import Optic.Core (Lens', lens)
import Pha (app)
import Lib.Game (init)
import Lib.Random (runRnd)
import Game.Baseball.Model (BaseballState, baseballState)
import Game.Baseball.View (view) as BaseballView
import Game.Nim.Model (NimState, nimState)
import Game.Nim.View (view) as NimView
import Game.Frog.Model (FrogState, frogState)
import Game.Frog.View (view) as FrogView

type RootState = {
    baseball :: BaseballState,
    nim :: NimState,
    frog :: FrogState
}

baseballLens :: Lens' RootState BaseballState
baseballLens = lens (_.baseball) (_{baseball = _})

nimLens :: Lens' RootState NimState
nimLens = lens (_.nim) (_{nim = _})

frogLens :: Lens' RootState FrogState
frogLens = lens (_.frog) (_{frog = _})

main :: Effect Unit
main = do
    baseballState' <- runRnd $ init baseballState
    nimState' <- runRnd $ init nimState
    frogState' <- runRnd $ init frogState
    let state = {
        baseball: baseballState,
        nim: nimState',
        frog: frogState'
    }
    app {
        init: state,
        view: \st -> FrogView.view frogLens st.frog,
        node: "root"
    }