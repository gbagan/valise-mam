module Lib.KonamiCode (konamiCode) where
import MamPrelude
import Data.Array (takeEnd)

codeSequence ∷ Array String
codeSequence = ["ArrowUp", "ArrowUp", "ArrowDown", "ArrowDown", "ArrowLeft", "ArrowRight", "ArrowLeft", "ArrowRight", "b", "a"]

konamiCode ∷ ∀m model. MonadState model m ⇒ Lens' model (Array String) → m Unit → String → m Unit
konamiCode lens onActivation key = do
  model ← get
  let seq = takeEnd 10 $ (model ^. lens) `snoc` key
  lens .= seq
  when (seq == codeSequence) onActivation