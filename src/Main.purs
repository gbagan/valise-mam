module Main where
import Effect (Effect)
import Prelude
import Pha (app)
import Optic.Core (Lens', lens, over, set, (^.))
import Lib.Random (Random, Seed(Seed), run)
import Game.Baseball.Model (example, BaseballState)
import Game.Baseball.View (view)

type MyState = { counter :: Int }

type RootState = {
    baseball :: BaseballState,
    seed :: Seed
}

root :: RootState
root = {
    baseball: example,
    seed: Seed 90192011
}

-- increment :: MyState -> MyState
-- increment st = st { counter = st.counter + 1}

--view st = div_ [] [
--    text $ show st.counter,
--    br,
--    button [
--        click $ action increment,
--        style "background-color" "yellow"
--    ] [text "Test"]
--]

-- state :: MyState
-- state = { counter: 0 }

mylens :: Lens' RootState BaseballState
mylens = lens (\r -> r.baseball) (\r x -> r{baseball = x})

rndlens :: Lens' RootState Seed
rndlens = lens (\r -> r.seed) (\r x -> r{seed = x})

laction :: (BaseballState -> BaseballState) -> RootState -> RootState
laction = over mylens

rndaction :: (BaseballState -> Random BaseballState) -> RootState -> RootState
rndaction f st = run rndlens st $ do
    x <- f $ st ^. mylens
    pure $ set mylens x st

main :: Effect Unit
main = do app {
    init: \x -> root,
    view: \st -> view laction rndaction st.baseball,
    node: "root"
}
