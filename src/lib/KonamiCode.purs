module Lib.KonamiCode where
import Prelude
import Data.Array (snoc, takeEnd)
import Effect.Class (liftEffect)
import Data.Lens (Lens', (^.), (.~))
import Data.String (joinWith)
import Pha.Action (Action(..))

codeSequence :: String
codeSequence = "ArrowUp ArrowUp ArrowDown ArrowDown ArrowLeft ArrowRight ArrowLeft ArrowRight b a"

konamiCode :: forall a. Lens' a (Array String) -> Action a -> Action a
konamiCode lens (Action onActivation) = Action \setState e state -> do
    let seq = state ^. lens # flip snoc  "" {-(e.key)-} # takeEnd 10
    st2 <- liftEffect $ setState (state # lens .~ seq)
    if joinWith " " seq == codeSequence then
        onActivation setState e st2
    else
        pure st2