module Lib.Update
  ( Env
  , UpdateMam
  , evalGen
  , getHash
  , module Exports
  , storageGet
  , storagePut
  ) where

import MamPrelude

import Effect.Aff.Class (class MonadAff)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Pha.Update (Update)
import Web.HTML (window)
import Web.HTML.Location as L
import Web.HTML.Window (localStorage, location)
import Web.Storage.Storage as Storage
import Control.Monad.Reader.Trans (ReaderT, ask)
import Control.Monad.Gen.Trans (GenState, runGen)
import Pha.Update (Update, delay) as Exports

type Env = { genState :: Ref GenState }

type UpdateMam model msg a = Update model msg (ReaderT Env Aff) a

evalGen ∷ ∀ model msg a. Gen a -> UpdateMam model msg a
evalGen g = do
  { genState } <- lift ask
  st <- liftEffect $ Ref.read genState
  let v /\ st' = runGen g st
  liftEffect $ Ref.write st' genState
  pure v

getHash ∷ ∀ model msg m. MonadAff m => Update model msg m String
getHash = liftEffect $ window >>= location >>= L.hash

storageGet ∷ ∀ m. MonadAff m => String → m (Maybe String)
storageGet name = liftEffect $ window >>= localStorage >>= Storage.getItem name

storagePut ∷ ∀ m. MonadAff m => String → String → m Unit
storagePut name value = liftEffect $ window >>= localStorage >>= Storage.setItem name value