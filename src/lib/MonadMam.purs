module Lib.MonadMam where

import MamPrelude
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Random (randomInt, random)

newtype MonadMam a = MonadMam (Aff a)

derive newtype instance Functor MonadMam
derive newtype instance Apply MonadMam
derive newtype instance Applicative MonadMam
derive newtype instance Bind MonadMam
instance Monad MonadMam
derive newtype instance MonadRec MonadMam
derive newtype instance MonadEffect MonadMam
derive newtype instance MonadAff MonadMam

instance MonadGen MonadMam where
    chooseInt a b = MonadMam $ liftEffect $ randomInt a b
    chooseFloat a b = MonadMam $ liftEffect $ random <#> \n -> a + (b - a) * n
    chooseBool = MonadMam $ liftEffect $ (_ == 0) <$> randomInt 0 1
    resize _ m = m
    sized f = f 0

eval :: MonadMam ~> Aff
eval (MonadMam m) = m