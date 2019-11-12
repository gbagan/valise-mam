module Game.Sansmot.Model where
import MyPrelude
import Data.Map(Map, empty) as M
import Pha.Action (Action, setState, getState, delay, DELAY)

data AnimationStep = Step Int String Int -- delay, type d'items, phase 

pythaAnimation :: Array AnimationStep
pythaAnimation = [
    Step 500 "a" 1,
    Step 200 "a" 2,
    Step 600 "b" 1,
    Step 200 "b" 2,
    Step 600 "c" 1,
    Step 200 "c" 2,
    Step 600 "d" 1,
    Step 200 "d" 2,
    Step 600 "e" 1
]

type State = {
    anim :: M.Map String Int,
    locked :: Boolean
    --dialog: null,
    --pages: 'main',
}

istate :: State
istate = {
    anim: M.empty,
    locked: false
}

_anim :: Lens' State (M.Map String Int)
_anim = lens (_.anim) (_{anim = _})

_locked :: Lens' State Boolean
_locked = lens (_.locked) (_{locked = _})

animateA :: ∀effs. Array AnimationStep -> Action State (delay :: DELAY | effs)
animateA animation = do
    unlessM (view _locked <$> getState) $ do
        setState $ (_anim .~ M.empty) ∘ (_locked .~ true)
        delay 1000
        for_ animation \(Step d key step) -> do
            delay d
            state <- getState
            setState $ _anim ∘ at key .~ Just step
        setState $ _locked .~ false
