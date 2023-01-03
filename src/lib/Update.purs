module Lib.Update
  ( Env
  , UpdateMam
  , delay
  , evalGen
  , getHash
  , module Exports
  , storageGet
  , storagePut
  )
  where

import MamPrelude

import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Pha.Update (Update)
import Web.HTML (window)
import Web.HTML.Location as L
import Web.HTML.Window (localStorage, location)
import Web.Storage.Storage as Storage
import Control.Monad.Reader.Trans (ReaderT, ask)
import Control.Monad.Gen.Trans (Gen, GenState, runGen)
import Pha.Update (Update) as Exports

type Env = { genState :: Ref GenState }

type UpdateMam st a = Update st (ReaderT Env Aff) a

evalGen :: ∀st a. Gen a -> UpdateMam st a
evalGen g = do
    {genState} <- lift ask
    state <- liftEffect $ Ref.read genState
    let v /\ state' = runGen g state
    liftEffect $ Ref.write state' genState
    pure v

getHash ∷ ∀st m. MonadAff m => Update st m String
getHash = liftEffect $ window >>= location >>= L.hash

storageGet ∷ ∀m. MonadAff m => String → m (Maybe String)
storageGet name = liftEffect $ window >>= localStorage >>= Storage.getItem name

storagePut ∷ ∀m. MonadAff m => String → String → m Unit
storagePut name value = liftEffect $ window >>= localStorage >>= Storage.setItem name value

delay :: ∀m. MonadAff m => Milliseconds → m Unit
delay = liftAff <<< Aff.delay