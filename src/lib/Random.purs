module Lib.Random where
import Prelude
import Math (sin)
import Effect (Effect)
import Data.Int (floor, toNumber)
import Data.Tuple (Tuple(Tuple), fst, uncurry)
import Data.Array.NonEmpty (NonEmptyArray, unsafeIndex, length) as N
import Data.Traversable (sequence)
import Data.Array (replicate, length, mapWithIndex, foldr)
import Partial.Unsafe (unsafePartial)
import Lib.Core (repeat, swap)

newtype Seed = Seed Number
newtype Random a = Random  (Seed -> Tuple a Seed)

instance monadRandom :: Monad Random

instance functorRandom :: Functor Random where
    map f (Random m) = Random (\seed -> let Tuple a seed2 = m seed in Tuple (f a) seed2)

instance applyRandom :: Apply Random where
    apply = ap

instance applicativeRandom :: Applicative Random where
    pure x = Random  $ Tuple x

instance bindRandom :: Bind Random where
    bind (Random m) f = Random \seed ->
        let Tuple res seed2 = m seed in
        let Random m2 = f res in
        m2 seed2


intFromSeed :: Seed -> Int -> Int
intFromSeed (Seed i) max = floor $ i * toNumber max

nextSeed :: Seed -> Seed
nextSeed (Seed x) = Seed $ (sin (x * 2819921.0) + 1.0) / 2.0

foreign import genSeed :: Effect Seed

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

randomPick :: forall a. N.NonEmptyArray a -> Random a
randomPick t =
    unsafePartial $ N.unsafeIndex t <$> (randomInt $ N.length t)

runRnd :: forall a. Random a -> Effect a
runRnd (Random m) = do
    seed <- genSeed
    pure $ fst $ m seed

