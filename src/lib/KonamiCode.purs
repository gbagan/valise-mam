module Lib.KonamiCode where
import Prelude
import Data.Array (snoc, takeEnd)
import Data.Lens (Lens', (^.), (.~))
import Data.String (joinWith)
import Pha.Action (Action, asyncAction)

codeSequence :: String
codeSequence = "ArrowUp ArrowUp ArrowDown ArrowDown ArrowLeft ArrowRight ArrowLeft ArrowRight b a"

konamiCode :: forall a. Lens' a (Array String) -> Action a -> String -> Action a
konamiCode lens onActivation key = asyncAction \{getState, updateState, dispatch} -> do
    state <- getState
    let seq = state ^. lens # flip snoc key # takeEnd 10
    _ <- updateState (lens .~ seq)
    if joinWith " " seq == codeSequence then
        dispatch onActivation
    else
        pure unit