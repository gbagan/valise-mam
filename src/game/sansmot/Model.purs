module Game.Sansmot.Model where
import MyPrelude
import Data.Map(Map, empty) as M
import Lib.Update (Update, get, modify, delay)

data Page = PythaPage | CarollPage

data AnimationStep = Step Int String Int -- delay, type d'items, phase 

pythaAnimation ∷ Array AnimationStep
pythaAnimation = 
    [   Step 500 "a" 1
    ,   Step 200 "a" 2
    ,   Step 600 "b" 1
    ,   Step 200 "b" 2
    ,   Step 600 "c" 1
    ,   Step 200 "c" 2
    ,   Step 600 "d" 1
    ,   Step 200 "d" 2
    ,   Step 600 "e" 1
    ]

carollAnimation ∷ Array AnimationStep
carollAnimation =
    [   Step 700 "a" 1
    ,   Step 700 "b" 1
    ,   Step 700 "c" 1
    ,   Step 700 "d" 1
    ,   Step 600 "e" 1
    ]


type State = {
    anim ∷ M.Map String Int,
    locked ∷ Boolean,
    --dialog: null,
    page ∷ Page
}

istate ∷ State
istate = 
    {   anim: M.empty
    ,   locked: false
    ,   page: CarollPage
    }

_anim ∷ Lens' State (M.Map String Int)
_anim = lens _.anim _{anim = _}
_locked ∷ Lens' State Boolean
_locked = lens _.locked _{locked = _}
_page ∷ Lens' State Page
_page = lens _.page _{page = _}

data Msg = SetPage Page | Animate (Array AnimationStep)

lockAction ∷ Update State → Update State
lockAction action = unlessM (get <#> _.locked) do
        modify _{locked = true}
        action
        modify _{locked = false}

update ∷ Msg → Update State
update (SetPage page) = modify \st →
    if st^._locked then
        st
    else
        st # _page .~ page # _anim .~ M.empty

update (Animate animation) = lockAction do
        modify $ _anim .~ M.empty 
        for_ animation \(Step d key step) → do
            delay d
            modify $ _anim ∘ at key .~ Just step
