module Lib.Random (Random, number, int, int', bool, arrayOf, shuffle, element, element', RandomF(..)) where
import Prelude
import Data.Maybe (Maybe, fromMaybe)
import Data.Unfoldable (replicateA)
import Data.Tuple (fst, snd)
import Data.Array (length, index, sortWith, zip)
import Data.Array.NonEmpty as NEA
import Control.Monad.Free (Free, liftF)

data RandomF a = RandomInt Int (Int → a) | RandomNumber (Number → a)
derive instance Functor RandomF
type Random = Free RandomF

-- | generate a random integer in the range [0, n - 1]
int' ∷ Int → Random Int
int' n = liftF (RandomInt n identity)

-- | generate a random integer in the range [n, m]
int ∷ Int → Int → Random Int
int n m
    | m < n = int m n
    | otherwise = int' (m + 1 - n) <#> (_ + n)

-- | generate a random number in the range [0, 1)
number ∷ Random Number
number = liftF (RandomNumber identity)

-- | generate a random boolean 
bool ∷ Random Boolean
bool = int' 2 <#> eq 0

arrayOf ∷ ∀a. Int → Random a → Random (Array a)
arrayOf = replicateA 

-- | randomly shuffle an array
shuffle ∷ ∀a. Array a → Random (Array a)
shuffle xs = do
    ns <- arrayOf (length xs) number
    pure (map snd (sortWith fst (zip ns xs)))

-- | randomly select an element from the array
element ∷ ∀a. NEA.NonEmptyArray a → Random a
element t = fromMaybe (NEA.head t) <$> NEA.index t <$> int' (NEA.length t)

-- | randomly select an element from the array
element' ∷ ∀a. Array a → Random (Maybe a)
element' t = index t <$> int' (length t)