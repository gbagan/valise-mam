module Game.Effs (EFFS, module E) where
import Pha.Effects.Delay (DELAY, delay, interpretDelay) as E
import Pha.Effects.Nav (NAV, interpretNav) as E
import Pha.Effects.Random (RNG, randomGenerate, randomly, interpretRng) as E

type EFFS = (rng ∷ E.RNG, delay ∷ E.DELAY, nav ∷ E.NAV)
