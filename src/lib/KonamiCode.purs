module Lib.KonamiCode (konamiCode) where
import MyPrelude
import Data.Array (takeEnd)
import Lib.Update (get)

codeSequence ∷ String
codeSequence = "ArrowUp ArrowUp ArrowDown ArrowDown ArrowLeft ArrowRight ArrowLeft ArrowRight b a"

konamiCode ∷ ∀m st. MonadState st m ⇒ Lens' st (Array String) → m Unit → String → m Unit
konamiCode lens onActivation key = do
    state ← get
    let seq = state ^. lens
                # (_ `snoc` key)
                # takeEnd 10
    lens .= seq
    when (joinWith " " seq == codeSequence) onActivation