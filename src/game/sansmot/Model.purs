module Game.Sansmot.Model where
import MyPrelude
import Data.Map(Map, empty) as M
import Pha.Action (Action, setState, getState, delay, DELAY)

data Page = PythaPage | CarollPage

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

carollAnimation :: Array AnimationStep
carollAnimation = [
    Step 700 "a" 1,
    Step 700 "b" 1,
    Step 700 "c" 1,
    Step 700 "d" 1,
    Step 600 "e" 1
]


type State = {
    anim :: M.Map String Int,
    locked :: Boolean,
    --dialog: null,
    page :: Page
}

istate :: State
istate = {
    anim: M.empty,
    locked: false,
    page: CarollPage
}

_anim :: Lens' State (M.Map String Int)
_anim = lens (_.anim) (_{anim = _})

_locked :: Lens' State Boolean
_locked = lens (_.locked) (_{locked = _})

_page :: Lens' State Page
_page = lens (_.page) (_{page = _})

setPageA :: ∀effs. Page -> Action State effs
setPageA page = unlessM (view _locked <$> getState) $ setState ((_page .~ page) ∘ (_anim .~ M.empty))

animateA :: ∀effs. Array AnimationStep -> Action State (delay :: DELAY | effs)
animateA animation = do
    unlessM (view _locked <$> getState) $ do
        setState $ (_anim .~ M.empty) ∘ (_locked .~ true)
        for_ animation \(Step d key step) -> do
            delay d
            state <- getState
            setState $ _anim ∘ at key .~ Just step
        setState $ _locked .~ false
