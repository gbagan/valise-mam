module Lib.KonamiCode (konamiCode) where
import MyPrelude
import Data.Array (takeEnd)
import Pha.Update (Update, get, modify)

codeSequence ∷ String
codeSequence = "ArrowUp ArrowUp ArrowDown ArrowDown ArrowLeft ArrowRight ArrowLeft ArrowRight b a"

konamiCode ∷ ∀st effs. Lens' st (Array String) → Update st effs → String → Update st effs
konamiCode lens onActivation key = do
    state ← get
    let seq = state ^. lens # flip snoc key # takeEnd 10
    modify $ lens .~ seq
    when (joinWith " " seq == codeSequence) onActivation