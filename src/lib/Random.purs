module Lib.Random where
import Prelude
import Optic.Core (Lens', (^.), set)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Traversable (sequence)
import Data.Array (replicate, length, mapWithIndex, foldr)
import Lib.Core (repeat, swap)

newtype Seed = Seed Int
newtype Random a = Random (Seed -> Tuple a Seed)

instance monadRandom :: Monad Random

instance functorRandom :: Functor Random where
    map f (Random m) = Random (\seed -> let Tuple a seed2 = m seed in Tuple (f a) seed2)

instance applyRandom :: Apply Random where
    apply = ap

instance applicativeRandom :: Applicative Random where
    pure x = Random $ \seed -> Tuple x seed

instance bindRandom :: Bind Random where
    bind (Random m) f = Random \seed ->
        let Tuple res seed2 = m seed in 
        let Random m2 = f res in
        m2 seed2




intFromSeed :: Seed -> Int -> Int
intFromSeed (Seed i) max = i `mod` max

foreign import nextSeed :: Seed -> Seed


randomInt :: Int -> Random Int
randomInt n = Random (\seed -> Tuple (intFromSeed seed n) (nextSeed seed))

randomInts :: Int -> Int -> Random (Array Int)
randomInts n x = sequence $ replicate n (randomInt x)

randomInts' :: Int -> (Int -> Int) -> Random (Array Int)
randomInts' n fn = sequence $ repeat n (\i -> randomInt $ fn i)

shuffle :: forall a. Array a -> Random (Array a)
shuffle array = do
    rnds <- randomInts' (n - 1) (\x -> n - x)
    pure (rnds 
        # mapWithIndex (\i j -> Tuple i (i + j))
        # foldr (uncurry swap) array
    )
    where n = length array

run :: forall a. Lens' a Seed -> a -> Random a -> a
run lens st (Random m) = set lens seed st' where Tuple st' seed = m $ st ^. lens