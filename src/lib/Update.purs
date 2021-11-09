module Lib.Update (class MonadDelay, delay, getHash, storageGet, storagePut, module Exports) where

import MyPrelude

import Effect.Class (liftEffect)
import Effect.Aff.Class (liftAff)
import Effect.Aff as Aff
import Pha.Update (Update)
import Web.HTML (window)
import Web.HTML.Location as L
import Web.HTML.Window (localStorage, location)
import Web.Storage.Storage as Storage

import Pha.Update (Update, get, modify_, put) as Exports

getHash ∷ ∀st. Update st String
getHash = liftEffect $ window >>= location >>= L.hash

storageGet ∷ ∀st. String → Update st (Maybe String)
storageGet name = liftEffect $ window >>= localStorage >>= Storage.getItem name

storagePut ∷ ∀st. String → String → Update st Unit
storagePut name value = liftEffect $ window >>= localStorage >>= Storage.setItem name value

class Monad m <= MonadDelay m where
    delay ∷ Milliseconds → m Unit

instance MonadDelay (Update s) where
    delay = liftAff <<< Aff.delay