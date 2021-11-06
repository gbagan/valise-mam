module Lib.Update (getHash, storageGet, storagePut, module Exports) where

import MyPrelude

import Effect.Class (liftEffect)
import Pha.Update (Update)
import Web.HTML (window)
import Web.HTML.Location as L
import Web.HTML.Window (localStorage, location)
import Web.Storage.Storage as Storage

import Pha.Update (Update, Update', get, modify_, put, delay) as Exports

getHash ∷ ∀st. Update st String
getHash = liftEffect $ window >>= location >>= L.hash

storageGet ∷ ∀st. String → Update st (Maybe String)
storageGet name = liftEffect $ window >>= localStorage >>= Storage.getItem name

storagePut ∷ ∀st. String → String → Update st Unit
storagePut name value = liftEffect $ window >>= localStorage >>= Storage.setItem name value