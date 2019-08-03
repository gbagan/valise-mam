module Main where
import Effect (Effect)
import Prelude
import Pha (app)
import Optic.Core (Lens', lens)
import Game.Baseball.Model (example, BaseballState)
import Game.Baseball.View (view)

type MyState = { counter :: Int }

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

mylens :: Lens' BaseballState BaseballState
mylens = lens (\x -> x) (\x a -> a)

main :: Effect Unit
main = do app {
    init: \x -> example,
    view: view mylens,
    node: "root"
}
