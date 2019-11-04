module Lib.KonamiCode (konamiCode) where
import MyPrelude
import Data.Array (takeEnd)
import Data.String (joinWith)
import Pha.Action (Action, asyncAction)

codeSequence :: String
codeSequence = "ArrowUp ArrowUp ArrowDown ArrowDown ArrowLeft ArrowRight ArrowLeft ArrowRight b a"

konamiCode :: forall a. Lens' a (Array String) -> Action a -> String -> Action a
konamiCode lens onActivation key = asyncAction \{updateState, dispatch} state-> do
    let seq = state ^. lens # flip snoc key # takeEnd 10
    st2 <- updateState (lens .~ seq)
    if joinWith " " seq == codeSequence then
        dispatch onActivation
    else
        pure st2