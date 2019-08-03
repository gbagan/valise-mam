module Lib.Random where
import Prelude

newtype Seed = Seed Int
data Rtuple a = Rtuple a Seed
newtype Random a = Random (Seed -> Rtuple a)

pure :: forall a. a -> Random a
pure x = Random $ \seed -> Rtuple x seed

bind :: forall a b. Random a -> (a -> Random b) -> Random b
bind (Random m) f = Random (\seed ->
    let Rtuple res seed2 = m seed in 
    let Random m2 = f res in
    m2 seed2        
)

intFromSeed :: Seed -> Int -> Int
intFromSeed (Seed i) max = i - max

nextSeed :: Seed -> Seed
nextSeed s = s


randomInt :: Int -> Random Int
randomInt n = Random (\seed -> Rtuple (intFromSeed seed n) (nextSeed seed))

--randomInts' :: Int -> (Int -> Int) -> Random (Array Int)
--randomInts' n fn = sequence $ repeat n (\i -> randomInt $ fn i)

--shuffle :: Array Int -> Random (Array Int)
--shuffle x = pure x