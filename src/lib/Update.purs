module Lib.Update (randomEval, randomly, getHash, storageGet, storagePut, module Exports) where

import MyPrelude

import Effect.Class (liftEffect)
import Effect.Random as R
import Control.Monad.Free (runFreeM)
import Lib.Random (Random, RandomF(..))
import Pha.Update (Update, get, put)
import Web.HTML (window)
import Web.HTML.Location as L
import Web.HTML.Window (localStorage, location)
import Web.Storage.Storage as Storage

import Pha.Update (Update, Update', get, modify_, put, delay) as Exports

randomEval ∷ ∀a st. Random a → Update st a
randomEval = liftEffect <<< eval where
    eval = runFreeM go 
    go (RandomInt m next) = R.randomInt 0 (m-1) <#> next
    go (RandomNumber next) = R.random <#> next

randomly ∷ ∀st. (st → Random st) → Update st Unit
randomly f = do
    st ← get
    st2 ← randomEval (f st)
    put st2

getHash ∷ ∀st. Update st String
getHash = liftEffect $ window >>= location >>= L.hash

storageGet ∷ ∀st. String → Update st (Maybe String)
storageGet name = liftEffect $ window >>= localStorage >>= Storage.getItem name

storagePut ∷ ∀st. String → String → Update st Unit
storagePut name value = liftEffect $ window >>= localStorage >>= Storage.setItem name value