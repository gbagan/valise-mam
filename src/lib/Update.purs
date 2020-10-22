module Lib.Update where

import MyPrelude

import Control.Monad.Free (Free, liftF, hoistFree, runFreeM)
import Data.Exists (Exists, mkExists, runExists)
import Effect (Effect)
import Lib.Random (Random)
import Pha.App.Router as Router
import Effect.Timer (setTimeout)
import Unsafe.Coerce (unsafeCoerce)

data GenWrapper a b = GenWrapper (Random b) (b → a)

data CommandF st a = 
      Get (st → a)
    | Modify (st → st) a
    | Delay Int a
    | Rng (Exists (GenWrapper a))
    | GoTo String a
    | StorageGet String (Maybe String → a)
    | StoragePut String String a

instance functorCommandF ∷ Functor (CommandF st) where
    map f (Get a) = Get (f <<< a)
    map f (Modify g a) = Modify g (f a)
    map f (Delay n a) = Delay n (f a)
    map f (Rng a) = Rng $ a # runExists \(GenWrapper d g) → mkExists (GenWrapper d (f <<< g))
    map f (GoTo url a) = GoTo url (f a)
    map f (StorageGet n a) = StorageGet n (f <<< a)
    map f (StoragePut n d a) = StoragePut n d (f a) 

type Command st = Free (CommandF st)
type Update st = Command st Unit

updateOver ∷ ∀st st'. Lens' st st' → Command st' ~> Command st
updateOver lens = hoistFree case _ of
    Get a → Get (a <<< view lens)
    Modify f a → Modify (lens %~ f) a
    x → unsafeCoerce x

interpret ∷ ∀st. {get ∷ Effect st, modify ∷ (st → st) → Effect Unit} → Update st → Effect Unit
interpret {get: get', modify: modify'} = runFreeM go
    where
    go (Get a) = get' <#> a
    go (Modify f a) = modify' f *> pure a
    -- go (Delay ms a) = void $ setTimeout ms (pure a)
    go (GoTo url a) = Router.goTo url *> pure a
    go (StorageGet n a) = storageGetImpl Nothing Just n <#> a
    go (StoragePut n v a) = storagePutImpl n v *> pure a
    go _ = unsafeCoerce 1

foreign import storagePutImpl :: String → String → Effect Unit 
foreign import storageGetImpl :: Maybe String → (String → Maybe String) → String → Effect (Maybe String)

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

goTo ∷ ∀st. String → Command st Unit
goTo url = liftF $ GoTo url unit

storageGet ∷ ∀st. String → Command st (Maybe String)
storageGet name = liftF $ StorageGet name identity

storagePut ∷ ∀st. String → String → Command st Unit
storagePut name value = liftF $ StoragePut name value unit