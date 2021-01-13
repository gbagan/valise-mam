module Lib.KonamiCode (konamiCode) where
import MyPrelude
import Data.Array (takeEnd)
import Lib.Update (Update, get)

codeSequence ∷ String
codeSequence = "ArrowUp ArrowUp ArrowDown ArrowDown ArrowLeft ArrowRight ArrowLeft ArrowRight b a"

konamiCode ∷ ∀st. Lens' st (Array String) → Update st Unit → String → Update st Unit
konamiCode lens onActivation key = do
    state ← get
    let seq = state ^. lens
                # (_ `snoc` key)
                # takeEnd 10
    lens .= seq
    when (joinWith " " seq == codeSequence) onActivation