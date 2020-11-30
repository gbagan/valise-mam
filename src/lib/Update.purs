module Lib.Update where

import MyPrelude

import Control.Monad.Free (Free, liftF, hoistFree, runFreeM, resume')
import Data.Exists (Exists, mkExists, runExists)
import Effect (Effect)
import Lib.Random (Random, RandomF(..))
import Effect.Timer (setTimeout)
import Effect.Random as R
import Web.HTML (window)
import Web.HTML.Window (localStorage, location)
import Web.HTML.Location as L
import Web.Storage.Storage as Storage
import Unsafe.Coerce (unsafeCoerce)

data GenWrapper a b = GenWrapper (Random b) (b → a)

data CommandF st a = 
      Get (st → a)
    | Modify (st → st) a
    | Delay Int a
    | Rng (Exists (GenWrapper a))
    | GetHash (String → a)
    | StorageGet String (Maybe String → a)
    | StoragePut String String a

instance functorCommandF ∷ Functor (CommandF st) where
    map f (Get a) = Get (f ∘ a)
    map f (Modify g a) = Modify g (f a)
    map f (Delay n a) = Delay n (f a)
    map f (Rng a) = Rng $ a # runExists \(GenWrapper d g) → mkExists (GenWrapper d (f ∘ g))
    map f (GetHash a) = GetHash (f ∘ a)
    map f (StorageGet n a) = StorageGet n (f ∘ a)
    map f (StoragePut n d a) = StoragePut n d (f a) 

type Command st = Free (CommandF st)
type Update st = Command st Unit

updateOver ∷ ∀st st'. Lens' st st' → Command st' ~> Command st
updateOver lens = hoistFree case _ of
    Get a → Get (a ∘ view lens)
    Modify f a → Modify (over lens f) a
    x → unsafeCoerce x

resume
  ∷ ∀ a b r. Functor r =>
  (r (Free r a) → b)
  → (a → b)
  → Free r a
  → b
resume k1 k2 = resume' (\x f → k1 (f <$> x)) k2

runCont
  ∷ ∀ m a b r
  . Functor r =>
  (r (m b) → m b)
  → (a → m b)
  → Free r a
  → m b
runCont k1 k2 = loop
  where
  loop ∷ Free r a → m b
  loop = resume (\b → k1 (loop <$> b)) k2

interpret ∷ ∀st. {get ∷ Effect st, modify ∷ (st → st) → Effect Unit} → Update st → Effect Unit
interpret {get: get', modify: modify'} = do
    runCont go $ const (pure unit)
        where
        go (Get cont) = get' >>= cont
        go (Modify f cont) = modify' f *> cont
        go (Rng cont) = runExists f cont where
            f :: forall b. GenWrapper (Effect Unit) b → Effect Unit
            f (GenWrapper rndData next) = evalRng rndData >>= next
        go (Delay ms cont) = void $ setTimeout ms cont
        go (GetHash cont) = window >>= location >>= L.hash >>= cont
        go (StorageGet n cont) = window >>= localStorage >>= Storage.getItem n >>= cont
        go (StoragePut n v cont) = (window >>= localStorage >>= Storage.setItem n v) *> cont


evalRng :: forall a. Random a → Effect a
evalRng = runFreeM go where
    go (RandomInt m next) = R.randomInt 0 (m-1) <#> next
    go (RandomNumber next) = R.random <#> next

get ∷ ∀st. Command st st
get = liftF $ Get identity

put ∷ ∀st. st → Command st Unit
put st = liftF $ Modify (const st) unit

modify ∷ ∀st. (st → st) → Command st Unit
modify fn = liftF $ Modify fn unit

delay ∷ ∀st. Int → Command st Unit
delay ms = liftF $ Delay ms unit

randomEval ∷ ∀a st. Random a → Command st a
randomEval d = liftF (Rng $ mkExists (GenWrapper d identity))

randomly ∷ ∀st. (st → Random st) → Update st
randomly f = do
    st <- get
    st2 <- randomEval (f st)
    put st2

getHash ∷ ∀st. Command st String
getHash = liftF $ GetHash identity

storageGet ∷ ∀st. String → Command st (Maybe String)
storageGet name = liftF $ StorageGet name identity

storagePut ∷ ∀st. String → String → Command st Unit
storagePut name value = liftF $ StoragePut name value unit