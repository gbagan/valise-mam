module Game.Effs (EFFS, module E, STORAGE, StorageF, interpretStorage, storageGet, storagePut) where
import Prelude
import Effect(Effect)
import Data.Maybe (Maybe(..))
import Pha.Effects.Delay (DELAY, delay, interpretDelay) as E
import Pha.Effects.Nav (NAV, interpretNav) as E
import Pha.Effects.Random (RNG, randomGenerate, randomly, interpretRng) as E
import Run (FProxy, Run, SProxy(..), lift)

data StorageF a = StorageGet String (Maybe String → a) | StoragePut String String a
derive instance functorStorage ∷ Functor StorageF
type STORAGE = FProxy StorageF
_storage = SProxy ∷ SProxy "storage"

storagePut ∷ ∀r. String → String → Run (storage ∷ STORAGE | r) Unit
storagePut key value = lift _storage (StoragePut key value unit)

storageGet ∷ ∀r. String → Run (storage ∷ STORAGE | r) (Maybe String)
storageGet key = lift _storage (StorageGet key identity)

type EFFS = (rng ∷ E.RNG, delay ∷ E.DELAY, nav ∷ E.NAV, storage ∷ STORAGE)

foreign import lsput :: String → String → Effect Unit 
foreign import lsget :: Maybe String → (String → Maybe String) → String → Effect (Maybe String)

-- | default implementation of the effect delay
interpretStorage ∷ StorageF (Effect Unit) → Effect Unit
interpretStorage (StorageGet key next) = lsget Nothing Just key >>= next
interpretStorage (StoragePut key value next) = lsput key value *> next