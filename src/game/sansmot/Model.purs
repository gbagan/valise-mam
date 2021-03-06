module Game.Sansmot.Model where
import MyPrelude
import Data.Map as Map
import Lib.Update (Update, get, modify_, delay)

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
_anim = prop (Proxy ∷ _ "anim")
_locked ∷ Lens' State Boolean
_locked = prop (Proxy ∷ _ "locked")
_page ∷ Lens' State Page
_page = prop (Proxy ∷ _ "page")

data Msg = SetPage Page | Animate (Array AnimationStep)

lockAction ∷ Update State Unit → Update State Unit
lockAction action = unlessM (get <#> _.locked) do
        modify_ _{locked = true}
        action
        modify_ _{locked = false}

update ∷ Msg → Update State Unit
update (SetPage page) = modify_ \st →
    if st^._locked then
        st
    else
        st # set _page page 
           # set _anim Map.empty

update (Animate animation) = lockAction do
        modify_ $ set _anim Map.empty 
        for_ animation \(Step d key step) → do
            delay (Milliseconds $ toNumber d)
            modify_ $ set (_anim ∘ at key) (Just step)
