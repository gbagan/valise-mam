module Game.Sansmot.Model where
import MyPrelude
import Data.Map as Map
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
    anim ∷ Map String Int,
    locked ∷ Boolean,
    --dialog: null,
    page ∷ Page
}

istate ∷ State
istate = 
    {   anim: Map.empty
    ,   locked: false
    ,   page: CarollPage
    }

_anim ∷ Lens' State (Map String Int)
_anim = prop (SProxy ∷ _ "anim")
_locked ∷ Lens' State Boolean
_locked = prop (SProxy ∷ _ "locked")
_page ∷ Lens' State Page
_page = prop (SProxy ∷ _ "page")

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
        st # set _page page 
           # set _anim Map.empty

update (Animate animation) = lockAction do
        modify $ set _anim Map.empty 
        for_ animation \(Step d key step) → do
            delay d
            modify $ set (_anim ∘ at key) (Just step)
