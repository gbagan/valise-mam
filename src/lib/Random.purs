module Lib.Random where
import Prelude
import Math (sin)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Data.Int (floor, toNumber)
import Data.Tuple (Tuple(Tuple), fst)
import Data.Traversable (sequence)
import Data.Array (length, mapWithIndex, foldl, unsafeIndex, insertAt)
import Partial.Unsafe (unsafePartial)

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

randomBool :: Random Boolean
randomBool = randomInt 2 <#> eq 0

shuffle :: ∀a. Array a -> Random (Array a)
shuffle array = do
    rnds <- sequence $ array # mapWithIndex \i x -> Tuple x <$> randomInt (i+1)
    pure $ rnds # foldl (\t (Tuple x i) -> t # insertAt i x # fromMaybe []) []

randomPick :: ∀a. Array a -> Maybe (Random a)
randomPick [] = Nothing
randomPick t = Just $ unsafePartial $ unsafeIndex t <$> (randomInt $ length t)

runRnd :: ∀a. Seed -> Random a -> a
runRnd seed (Random m) = fst $ m seed
