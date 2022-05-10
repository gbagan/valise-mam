module Lib.Update
  ( UpdateMam
  , delay
  , getHash
  , module Exports
  , storageGet
  , storagePut
  )
  where

import MamPrelude

import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff as Aff
import Pha.Update (Update)
import Web.HTML (window)
import Web.HTML.Location as L
import Web.HTML.Window (localStorage, location)
import Web.Storage.Storage as Storage

import Pha.Update (Update) as Exports
import Lib.MonadMam (MonadMam)

type UpdateMam st = Update st MonadMam Unit 

getHash ∷ ∀st m. MonadAff m => Update st m String
getHash = liftEffect $ window >>= location >>= L.hash

storageGet ∷ ∀m. MonadAff m => String → m (Maybe String)
storageGet name = liftEffect $ window >>= localStorage >>= Storage.getItem name

storagePut ∷ ∀m. MonadAff m => String → String → m Unit
storagePut name value = liftEffect $ window >>= localStorage >>= Storage.setItem name value

delay :: ∀m. MonadAff m => Milliseconds → m Unit
delay = liftAff <<< Aff.delay